#!/usr/bin/env python3
"""Collect media URLs from an already logged-in course page using Playwright."""
import argparse
import math
import re
from pathlib import Path
from urllib.parse import urlsplit
from mooc import save_json


CATALOG_JS = '''() => Array.from(document.querySelectorAll('li.video')).map(row => ({
  number: row.querySelector('.hour')?.textContent.trim(),
  title: row.querySelector('span.catalogue_title[title]')?.getAttribute('title'),
  duration: row.querySelector('.time')?.textContent.trim()
})).filter(item => item.number && item.title && item.duration)'''

READY_JS = '''({number, previous}) => {
  const row = document.querySelector('li.video.current_play');
  const video = document.querySelector('#vjs_container_html5_api');
  return row?.querySelector('.hour')?.textContent.trim() === number
    && video?.readyState >= 1 && !!video.currentSrc
    && (!previous || video.currentSrc !== previous)
    && video.currentSrc === video.getAttribute('src');
}'''


def catalog_from_page(page):
    rows = page.evaluate(CATALOG_JS)
    if not rows or len({r['number'] for r in rows}) != len(rows):
        raise RuntimeError('No usable/unique course rows; confirm you are on a course playback page')
    return rows


def collect_page(page, output, timeout=30000):
    from playwright.sync_api import TimeoutError as PlaywrightTimeout
    catalog = catalog_from_page(page)
    records = []
    initial = page.evaluate('''() => ({
        number: document.querySelector('li.video.current_play .hour')?.textContent.trim(),
        url: document.querySelector('#vjs_container_html5_api')?.currentSrc
    })''')
    previous_url = initial.get('url') if initial.get('number') != catalog[0]['number'] else None
    for item in catalog:
        # Scope by exact chapter number; lesson/group titles may be duplicates.
        row = page.locator('li.video').filter(
            has=page.locator('.hour').filter(has_text=re.compile('^' + re.escape(item['number']) + '$')))
        target = row.locator('span.catalogue_title[title]')
        for attempt in range(2):
            target.click(timeout=timeout)
            try:
                page.wait_for_function(READY_JS, arg={'number': item['number'], 'previous': previous_url}, timeout=timeout)
                state = page.locator('#vjs_container_html5_api').evaluate(
                    'v => ({url:v.currentSrc, duration:v.duration})')
                if previous_url and state['url'] == previous_url:
                    raise RuntimeError('Same URL as previous lesson; stopped to avoid stale mapping')
                break
            except PlaywrightTimeout:
                # One retry only when the UI is visibly still on a different lesson.
                active = page.locator('li.video.current_play .hour').all_text_contents()
                if attempt == 0 and active and active[0].strip() != item['number']:
                    continue
                raise RuntimeError(f"{item['number']}: page did not become ready. Check login, loading or visible CAPTCHA manually.")
        if urlsplit(state['url']).scheme != 'https':
            raise RuntimeError('Expected HTTPS media URL; blob/segmented sources need separate investigation')
        expected = sum(int(n) * factor for n, factor in zip(item['duration'].split(':'), [3600, 60, 1]))
        if not isinstance(state['duration'], (int, float)) or not math.isfinite(state['duration']) or abs(state['duration'] - expected) > 5:
            raise RuntimeError(f"{item['number']}: media duration differs from catalog; refused stale mapping")
        records.append(dict(item, url=state['url'], media_duration=state['duration']))
        save_json(output, records)
        previous_url = state['url']
        # Stop playback after reading metadata; this script does not complete course tests.
        page.locator('#vjs_container_html5_api').evaluate('v => v.pause()')
        print(f"{item['number']} {item['title']}", flush=True)
    return records


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--cdp', default='http://127.0.0.1:9222')
    parser.add_argument('--course-url', required=True, help='Exact URL of the already open course playback page')
    parser.add_argument('--output', type=Path, default=Path('catalog.json'))
    parser.add_argument('--timeout', type=int, default=30000)
    args = parser.parse_args()
    if urlsplit(args.cdp).hostname not in ('127.0.0.1', 'localhost', '::1'):
        parser.error('Use a loopback CDP address; use an SSH tunnel for another host')
    if args.output.exists():
        parser.error('Output already exists; choose a new file to preserve the old collection')
    from playwright.sync_api import sync_playwright
    with sync_playwright() as playwright:
        browser = playwright.chromium.connect_over_cdp(args.cdp)
        pages = [p for context in browser.contexts for p in context.pages if p.url == args.course_url]
        if len(pages) != 1:
            raise RuntimeError(f'Expected exactly one matching open page; found {len(pages)}')
        records = collect_page(pages[0], args.output, args.timeout)
        print(f'Saved {len(records)} lessons to {args.output}')
        # Exiting Playwright disconnects; do not close the user browser/context/page.


if __name__ == '__main__':
    main()
