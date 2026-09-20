"""No external network: simulate Referer, Range, resume and damaged files."""
import http.server
import json
import struct
import sys
import tempfile
import threading
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
import mooc


def box(kind, payload):
    return struct.pack('>I4s', len(payload) + 8, kind) + payload


DATA = box(b'ftyp', b'isom' + b'\0' * 20) + box(b'moov', b'') + box(b'mdat', bytes(range(256)) * 1024)


class Handler(http.server.BaseHTTPRequestHandler):
    def log_message(self, *args):
        pass

    def do_GET(self):
        if self.headers.get('Referer') != mooc.REFERER:
            self.send_error(403)
            return
        import re
        requested = self.headers.get('Range', '')
        match = re.fullmatch(r'bytes=(\d+)-(\d*)', requested)
        start = int(match[1]) if match else 0
        end = int(match[2]) if match and match[2] else len(DATA) - 1
        if start >= len(DATA):
            self.send_error(416)
            return
        chunk = DATA[start:end + 1]
        self.send_response(206 if match else 200)
        self.send_header('Content-Length', str(len(chunk)))
        self.send_header('Content-Type', 'video/mp4')
        self.send_header('ETag', '"fixture-v1"')
        if match:
            self.send_header('Content-Range', f'bytes {start}-{end}/{len(DATA)}')
        self.end_headers()
        self.wfile.write(chunk)


class DownloaderTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.server = http.server.ThreadingHTTPServer(('127.0.0.1', 0), Handler)
        cls.thread = threading.Thread(target=cls.server.serve_forever, daemon=True)
        cls.thread.start()
        cls.url = f'http://127.0.0.1:{cls.server.server_port}/video.mp4'

    @classmethod
    def tearDownClass(cls):
        cls.server.shutdown()
        cls.server.server_close()
        cls.thread.join()

    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.dest = Path(self.temp.name)
        self.item = dict(number='1.1', title='sample', url=self.url)

    def test_referer_diagnosis(self):
        self.assertEqual(mooc.probe(self.url, None)['http'], '403')
        result = mooc.probe(self.url)
        self.assertTrue(result['ok'])
        self.assertEqual(result['bytes'], len(DATA))

    def test_download_and_existing(self):
        info = mooc.probe(self.url)
        target, status = mooc.download_one(self.item, self.dest, info, mooc.REFERER)
        self.assertEqual(status, 'downloaded')
        self.assertEqual(target.read_bytes(), DATA)
        self.assertEqual(mooc.download_one(self.item, self.dest, info, mooc.REFERER)[1], 'existing')

    def test_resume_matches_full(self):
        info = mooc.probe(self.url)
        target = self.dest / mooc.filename(self.item)
        target.with_suffix('.mp4.part').write_bytes(DATA[:1024])
        mooc.save_json(target.with_suffix('.mp4.part.json'), dict(url=self.url, bytes=info['bytes'], etag=info['etag']))
        completed, _ = mooc.download_one(self.item, self.dest, info, mooc.REFERER)
        self.assertEqual(completed.read_bytes(), DATA)
        self.assertFalse(target.with_suffix('.mp4.part').exists())

    def test_unidentified_partial_preserved(self):
        part = (self.dest / mooc.filename(self.item)).with_suffix('.mp4.part')
        part.write_bytes(b'old')
        with self.assertRaises(ValueError):
            mooc.download_one(self.item, self.dest, mooc.probe(self.url), mooc.REFERER)
        self.assertEqual(part.read_bytes(), b'old')

    def test_same_size_corrupt_final_refused(self):
        target = self.dest / mooc.filename(self.item)
        target.write_bytes(b'x' * len(DATA))
        with self.assertRaises(ValueError):
            mooc.download_one(self.item, self.dest, mooc.probe(self.url), mooc.REFERER)
        self.assertEqual(target.read_bytes(), b'x' * len(DATA))

    def test_truncated_mp4_refused(self):
        target = self.dest / 'broken.mp4'
        target.write_bytes(DATA[:-1])
        with self.assertRaises(ValueError):
            mooc.validate_mp4(target)

    def test_unsafe_number_refused(self):
        manifest = self.dest / 'bad.json'
        manifest.write_text(json.dumps([dict(number='../1', title='bad', url='https://example.com/x.mp4')]))
        with self.assertRaises(ValueError):
            mooc.load_catalog(manifest)


if __name__ == '__main__':
    unittest.main()
