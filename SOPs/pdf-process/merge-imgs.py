import requests
from PIL import Image
import os
import time
from requests.adapters import HTTPAdapter
from urllib3.util.retry import Retry
from datetime import datetime
from shutil import rmtree

# ==================== 配置区 ====================
base_url = (
    "https://s3.cldisk.com/sv-w9/doc/66/2e/ed/4529279618c891bf9bdbec0eaf2806b2/thumb/"
)
start_page = 1  # 从第几页开始（通常是1）
folder = "downloaded_images"  # 图片保存文件夹
output_pdf = f"{datetime.now().strftime('%Y%m%d_%H%M%S')}.pdf"  # 输出 PDF 文件名

# 伪装成 Chrome 浏览器的完整 headers
headers = {
    "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/131.0.0.0 Safari/537.36",
    "Accept": "image/avif,image/webp,image/apng,image/svg+xml,image/*,*/*;q=0.8",
    "Accept-Language": "zh-CN,zh;q=0.9,en;q=0.8",
    "Accept-Encoding": "gzip, deflate, br",
    "Referer": "https://cldisk.com/",  # 重要：模拟从主站来的请求，很多服务器会检查
    "Connection": "keep-alive",
    "Sec-Fetch-Dest": "image",
    "Sec-Fetch-Mode": "no-cors",
    "Sec-Fetch-Site": "cross-site",
}

# 检查断点续传一致性
base_url_file_path = os.path.join(folder, "base_url.txt")
if os.path.exists(base_url_file_path):
    with open(base_url_file_path, "r", encoding="utf-8") as f:
        saved_url = f.read().strip()
    if saved_url != base_url:
        print("检测到 base_url 不一致，清除旧数据并重新开始...")
        rmtree(folder, ignore_errors=True)
elif os.path.exists(folder):
    print("检测到下载目录存在但缺少标识文件，清除并重新开始...")
    rmtree(folder, ignore_errors=True)

os.makedirs(folder, exist_ok=True)
# 记录当前的 base_url
with open(base_url_file_path, "w", encoding="utf-8") as f:
    f.write(base_url)

# 创建带重试的 session
session = requests.Session()
retry = Retry(total=3, backoff_factor=1, status_forcelist=[500, 502, 503, 504])
adapter = HTTPAdapter(max_retries=retry)
session.mount("http://", adapter)
session.mount("https://", adapter)
session.headers.update(headers)

# 第一步：批量下载图片（支持断点续传，直到不存在）
page = start_page
downloaded_files = []

print("开始下载图片（已添加延时和重试，更稳定）...")
while True:
    file_path = os.path.join(folder, f"{page}.png")

    # 如果已经下载过，跳过
    if os.path.exists(file_path):
        print(f"已存在，跳过: {page}.png")
        downloaded_files.append(file_path)
        page += 1
        continue

    url = base_url + str(page) + ".png"

    success = False
    for attempt in range(3):  # 重试3次
        try:
            response = session.get(url, timeout=30)
            if (
                response.status_code == 200 and len(response.content) > 1000
            ):  # 简单检查内容是否正常（避免空图）
                with open(file_path, "wb") as f:
                    f.write(response.content)
                print(f"下载成功: {page}.png (尝试 {attempt+1})")
                downloaded_files.append(file_path)
                success = True
                break
            else:
                print(
                    f"第 {page} 页无内容或错误 (状态码 {response.status_code})，可能已结束"
                )
                success = False
                break
        except Exception as e:
            print(f"第 {page} 页下载出错 (尝试 {attempt+1}): {e}")
            time.sleep(2)  # 出错了等久点

    if not success:
        print(f"下载结束：连续失败，第 {page} 页可能不存在")
        break

    # 随机延时，伪装人类行为
    # delay = random.uniform(delay_min, delay_max)
    # print(f"等待 {delay:.1f} 秒...")
    # time.sleep(delay)

    page += 1

# 第二步：合并成 PDF（不变）
if not downloaded_files:
    print("没有下载到任何图片，退出。")
else:
    print("\n开始合并 PDF...")
    downloaded_files.sort(key=lambda x: int(os.path.basename(x).split(".")[0]))

    images = []
    for file_path in downloaded_files:
        try:
            img = Image.open(file_path)
            img = img.convert("RGB")
            images.append(img)
        except Exception as e:
            print(f"打开图片失败 {file_path}: {e}")

    if images:
        images[0].save(output_pdf, save_all=True, append_images=images[1:])
        print(f"\n合并完成！PDF 已保存为: {output_pdf}")
        print(f"共 {len(images)} 页")
    else:
        print("没有有效的图片可合并。")

print("全部完成！")
