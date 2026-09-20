"""可选的真浏览器集成测试。

媒体样本在测试运行时由 ffmpeg 现场合成，仓库内不存放任何 .mp4/.json 等数据文件。
如需复用现成样本，可设置 ZHS_TEST_MEDIA 指向一个约 3 分钟的 MP4。
"""
import importlib.util
import json
import os
import shutil
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from collect import collect_page

CHAPTER_TIME = '00:03:01'   # 与 fixture 页面中的 .time 一致
DURATION = 181              # collect.py 的时长核对容差为 ±5 秒


def build_media(directory):
    """优先用 ZHS_TEST_MEDIA；否则用 ffmpeg 现场合成。失败返回 None。"""
    override = os.environ.get('ZHS_TEST_MEDIA')
    if override:
        path = Path(override)
        return path.read_bytes() if path.is_file() else None
    if not shutil.which('ffmpeg'):
        return None
    target = Path(directory) / 'sample.mp4'
    command = ['ffmpeg', '-hide_banner', '-loglevel', 'error', '-y',
               '-f', 'lavfi', '-i', f'testsrc=size=64x64:rate=2:duration={DURATION}',
               '-f', 'lavfi', '-i', 'anullsrc=r=8000:cl=mono',
               '-shortest', '-c:v', 'libx264', '-preset', 'ultrafast', '-crf', '51',
               '-pix_fmt', 'yuv420p', '-c:a', 'aac', '-b:a', '8k', str(target)]
    try:
        subprocess.run(command, check=True, capture_output=True, timeout=300)
    except (OSError, subprocess.SubprocessError):
        return None
    return target.read_bytes() if target.is_file() else None


class CollectorTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls._tmp = tempfile.TemporaryDirectory()
        cls.media = build_media(cls._tmp.name)
        if not (cls.media and importlib.util.find_spec('playwright') and shutil.which('google-chrome')):
            cls._tmp.cleanup()
            raise unittest.SkipTest(
                'Optional: Playwright, system Google Chrome and ffmpeg (or ZHS_TEST_MEDIA) required')

    @classmethod
    def tearDownClass(cls):
        cls._tmp.cleanup()

    def test_duplicate_titles_and_media_transition(self):
        from playwright.sync_api import sync_playwright
        data = self.media
        with tempfile.TemporaryDirectory() as directory, sync_playwright() as playwright:
            browser = playwright.chromium.launch(executable_path=shutil.which('google-chrome'),
                                                headless=True, chromium_sandbox=True)
            try:
                page = browser.new_page()
                page.route('**/*', lambda route: route.fulfill(status=200, content_type='video/mp4', body=data))
                page.set_content(f'''
                    <span class="catalogue_title" title="Same title">Same title</span>
                    <ul>
                      <li class="video"><b class="hour">1.1</b><span class="catalogue_title" title="Same title">Same title</span><span class="time">{CHAPTER_TIME}</span></li>
                      <li class="video"><b class="hour">1.2</b><span class="catalogue_title" title="Same title">Same title</span><span class="time">{CHAPTER_TIME}</span></li>
                    </ul>
                    <video id="vjs_container_html5_api" preload="metadata"></video>
                    <script>
                    document.querySelectorAll('li.video').forEach((row, index) => {{
                      row.onclick = () => {{
                        document.querySelectorAll('li.video').forEach(r => r.classList.remove('current_play'));
                        row.classList.add('current_play');
                        const v = document.querySelector('video');
                        setTimeout(() => {{v.src = 'https://fixture.invalid/video-' + index + '.mp4'; v.load();}}, 100);
                      }};
                    }});
                    </script>
                ''')
                output = Path(directory) / 'catalog.json'
                rows = collect_page(page, output, timeout=15000)
                self.assertEqual([r['number'] for r in rows], ['1.1', '1.2'])
                self.assertNotEqual(rows[0]['url'], rows[1]['url'])
                self.assertEqual(len(json.loads(output.read_text())), 2)
                self.assertTrue(page.locator('video').evaluate('v => v.paused'))
            finally:
                browser.close()


if __name__ == '__main__':
    unittest.main()
