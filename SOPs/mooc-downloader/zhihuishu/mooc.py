#!/usr/bin/env python3
"""Linux Zhihuishu MP4 downloader: Python standard library + curl."""
import argparse
import csv
import hashlib
import json
import os
import re
import shutil
import struct
import subprocess
import sys
import tempfile
from pathlib import Path
from urllib.parse import urlsplit

ROOT = Path(__file__).resolve().parent
REFERER = 'https://studyvideoh5.zhihuishu.com/'
BASE = 'https://wsvideo.zhihuishu.com/zhs/zhihuishu_creatCourse_h5/UPLOADVIDEO/'


def save_json(path, value):
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    pending = path.with_name(path.name + '.tmp')
    pending.write_text(json.dumps(value, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
    pending.replace(path)


def load_catalog(path):
    path = Path(path)
    if path.suffix == '.json':
        items = json.loads(path.read_text(encoding='utf-8-sig'))
    else:
        with path.open(encoding='utf-8-sig', newline='') as stream:
            items = list(csv.DictReader(stream, delimiter='|' if path.suffix == '.psv' else ','))
    seen = set()
    for item in items:
        number = item.get('number', '')
        if not re.fullmatch(r'\d+(?:\.\d+)+', number) or number in seen:
            raise ValueError(f'Invalid or duplicate chapter number: {number!r}')
        seen.add(number)
        if not item.get('title'):
            raise ValueError(f'Missing title: {number}')
        item['url'] = item.get('url') or BASE + item['path']
        if urlsplit(item['url']).scheme != 'https':
            raise ValueError(f'Expected HTTPS media URL: {number}')
    if not items:
        raise ValueError('Empty catalog')
    return items


def filename(item):
    title = re.sub(r'[\x00-\x1f<>:"/\\|?*]', '_', item['title']).strip(' .')
    return '.'.join(f'{int(n):02d}' for n in item['number'].split('.')) + '-' + title + '.mp4'


def probe(url, referer=REFERER):
    """Bounded 32-byte request; no Cookie or credentials are exported."""
    with tempfile.TemporaryDirectory(prefix='zhs-probe-') as directory:
        header, body = Path(directory) / 'headers', Path(directory) / 'body'
        command = ['curl', '--silent', '--show-error', '--fail', '--retry', '2',
                   '--connect-timeout', '15', '--max-time', '45', '--max-filesize', '4096',
                   '--range', '0-31', '--dump-header', str(header), '--output', str(body),
                   '--write-out', '%{http_code}']
        if referer:
            command += ['--referer', referer]
        run = subprocess.run(command + [url], capture_output=True, text=True, timeout=180)
        headers = header.read_text(errors='replace') if header.exists() else ''
        data = body.read_bytes() if body.exists() else b''
        ranges = re.findall(r'^Content-Range:\s*bytes 0-31/(\d+)\s*$', headers, re.I | re.M)
        etags = re.findall(r'^ETag:\s*(.+?)\s*$', headers, re.I | re.M)
        valid = run.returncode == 0 and run.stdout.strip() == '206' and bool(ranges)
        valid = valid and len(data) == 32 and data[4:8] == b'ftyp'
        return dict(ok=bool(valid), http=run.stdout.strip(), curl_exit=run.returncode,
                    bytes=int(ranges[-1]) if ranges else None,
                    etag=etags[-1] if etags else None,
                    error=run.stderr.strip() if not valid else None)


def validate_mp4(path, expected=None):
    """Validate top-level box bounds, not frame-by-frame decoding."""
    path = Path(path)
    size = path.stat().st_size
    if expected is not None and size != expected:
        raise ValueError(f'Size mismatch: {size} != {expected}')
    boxes = []
    with path.open('rb') as stream:
        while stream.tell() < size:
            start = stream.tell()
            header = stream.read(8)
            if len(header) != 8:
                raise ValueError('Truncated MP4 box header')
            length, kind = struct.unpack('>I4s', header)
            minimum = 8
            if length == 1:
                extended = stream.read(8)
                if len(extended) != 8:
                    raise ValueError('Truncated extended MP4 box header')
                length, = struct.unpack('>Q', extended)
                minimum = 16
            elif length == 0:
                length = size - start
            if length < minimum or start + length > size:
                raise ValueError('Invalid/truncated MP4 box payload')
            boxes.append(kind)
            stream.seek(start + length)
    if not {b'ftyp', b'moov', b'mdat'}.issubset(boxes):
        raise ValueError('MP4 is missing ftyp, moov or mdat')
    return size


def sha256(path):
    with Path(path).open('rb') as stream:
        return hashlib.file_digest(stream, 'sha256').hexdigest()


def download_one(item, destination, info, referer):
    target = destination / filename(item)
    part = target.with_suffix('.mp4.part')
    metadata = target.with_suffix('.mp4.part.json')
    identity = dict(url=item['url'], bytes=info['bytes'], etag=info['etag'])
    if target.exists():
        validate_mp4(target, info['bytes'])
        return target, 'existing'
    if part.exists():
        if not metadata.exists() or json.loads(metadata.read_text()) != identity:
            raise ValueError('Partial-file identity differs or is missing; preserve it and use another destination')
        if part.stat().st_size > info['bytes']:
            raise ValueError('Partial file is larger than server file')
    else:
        save_json(metadata, identity)
    if not part.exists() or part.stat().st_size < info['bytes']:
        run = subprocess.run(['curl', '--silent', '--show-error', '--fail', '--retry', '3',
                              '--connect-timeout', '15', '--max-time', '1800',
                              '--continue-at', '-', '--referer', referer,
                              '--output', str(part), item['url']])
        if run.returncode:
            raise RuntimeError(f'curl exit {run.returncode}; partial file retained for resume')
    validate_mp4(part, info['bytes'])
    # Hard-link promotion never overwrites a concurrently created final file.
    os.link(part, target)
    part.unlink()
    metadata.unlink(missing_ok=True)
    return target, 'downloaded'


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('action', choices=['diagnose', 'probe', 'download', 'verify'])
    parser.add_argument('--manifest', type=Path, required=True,
                        help='Chapter manifest (.psv/.csv/.json); generate it with enumerate.py or collect.py')
    parser.add_argument('--dest', type=Path, default=ROOT / 'downloads')
    parser.add_argument('--referer', default=REFERER)
    parser.add_argument('--only', help='Comma-separated chapter numbers, e.g. 1.1,3.3')
    parser.add_argument('--report', type=Path)
    parser.add_argument('--ffprobe', action='store_true', help='Read container/stream metadata using ffprobe')
    args = parser.parse_args()
    if not shutil.which('curl'):
        parser.error('curl is required')
    if args.ffprobe and not shutil.which('ffprobe'):
        parser.error('ffprobe not found; omit --ffprobe or install ffmpeg')
    catalog = load_catalog(args.manifest)
    if args.only:
        selected = set(args.only.split(','))
        missing = selected - {item['number'] for item in catalog}
        if missing:
            parser.error(f'Unknown chapter numbers: {sorted(missing)}')
        catalog = [item for item in catalog if item['number'] in selected]
    if args.action == 'diagnose':
        item = catalog[0]
        result = dict(number=item['number'], without_referer=probe(item['url'], None),
                      with_referer=probe(item['url'], args.referer))
        print(json.dumps(result, ensure_ascii=False, indent=2))
        if args.report:
            save_json(args.report, result)
        return 0 if result['with_referer']['ok'] else 1
    args.dest.mkdir(parents=True, exist_ok=True)
    report = args.report or args.dest / (args.action + '-results.json')
    results = []
    for item in catalog:
        row = dict(number=item['number'], title=item['title'], url=item['url'])
        try:
            info = probe(item['url'], args.referer)
            if not info['ok']:
                raise RuntimeError(f'Media probe failed: {info}')
            row.update(bytes=info['bytes'], etag=info['etag'])
            target = args.dest / filename(item)
            if args.action == 'download':
                target, row['status'] = download_one(item, args.dest, info, args.referer)
            elif args.action == 'verify':
                validate_mp4(target, info['bytes'])
                row['status'] = 'verified'
            else:
                row['status'] = 'accessible'
            if args.action != 'probe':
                row.update(file=str(target.resolve()), sha256=sha256(target), mp4_structure=True)
                if args.ffprobe:
                    run = subprocess.run(['ffprobe', '-v', 'error', '-show_entries',
                                          'format=duration:stream=codec_type,codec_name,width,height',
                                          '-of', 'json', str(target)], capture_output=True, text=True, check=True)
                    row['ffprobe'] = json.loads(run.stdout)
            print(f"{item['number']} {row['status']} {info['bytes']} bytes", flush=True)
        except (OSError, ValueError, RuntimeError, subprocess.SubprocessError) as error:
            row.update(status='failed', error=str(error))
            print(f"{item['number']} FAILED: {error}", file=sys.stderr, flush=True)
        results.append(row)
        save_json(report, results)
    failed = sum(r['status'] == 'failed' for r in results)
    print(f'{len(results)} processed; {failed} failed; report: {report}')
    return bool(failed)


if __name__ == '__main__':
    try:
        sys.exit(main())
    except KeyboardInterrupt:
        print('Interrupted. Partial files retained; rerun the same command to resume.', file=sys.stderr)
        sys.exit(130)
