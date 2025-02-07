import csv
import os
import re
from icalendar import Calendar, Event, Alarm
from datetime import datetime, timedelta
import pytz

# 第一个教学周的周一日期(2025 年 2 月 17 日)
first_monday = datetime(2025, 2, 17, tzinfo=pytz.timezone("Asia/Shanghai"))

# 获取脚本所在目录
script_dir = "/home/wsdlly02/Documents/my-codes/Static"
file_path = os.path.join(script_dir, "class-schedule.csv")

# 读取课程表
with open(file_path, "r", encoding="utf-8") as file:
    reader = csv.reader(file)
    rows = list(reader)

# 创建日历对象
cal = Calendar()
cal.add("prodid", "-//Class Schedule 2025//example.com//")
cal.add("version", "2.0")


def parse_weeks(weeks_str):
    """解析离散周次(如 3, 11-12)"""
    weeks = []
    for part in re.split(r"[\s,]+", weeks_str):  # 支持空格或逗号分隔
        if "-" in part:
            start, end = map(int, part.split("-"))
            weeks.extend(range(start, end + 1))
        else:
            weeks.append(int(part))
    return weeks


# 解析课程表
for row in rows[1:]:  # 跳过表头
    time_slot = row[0]  # 时间段(如 8:20-9:05)
    start_time_str, end_time_str = time_slot.split("-")

    # 遍历星期一到星期五(仅处理前5列)
    for day_idx in range(5):  # 0=周一, 4=周五
        cell = row[day_idx + 1]  # CSV列索引从1到5对应周一到周五
        if cell.strip() == "无":
            continue  # 跳过无课的单元格

        # 分割同一单元格内的多门课程
        courses = cell.split(";")
        for course in courses:
            course = course.strip()
            if not course:
                continue

            # 提取教师、课程名、周次、教室
            try:
                teacher, rest = course.split(maxsplit=1)
                course_name, details = rest.split("(", 1)
                details = details.rstrip(")")

                # 分离周次和教室
                if ":" in details:
                    weeks_str, location = details.split(":", 1)
                else:
                    weeks_str = details
                    location = "未指定教室"

                # 解析离散周次
                weeks = parse_weeks(weeks_str)

                # 生成每个周次的事件
                for week in weeks:
                    # 计算当前周的星期几(周一=0，周五=4)
                    class_date = first_monday + timedelta(weeks=week - 1, days=day_idx)

                    # 设置课程时间(UTC+8)
                    start_time = datetime.strptime(
                        f"{class_date.date()} {start_time_str}", "%Y-%m-%d %H:%M"
                    ).replace(tzinfo=pytz.timezone("Asia/Shanghai"))
                    end_time = datetime.strptime(
                        f"{class_date.date()} {end_time_str}", "%Y-%m-%d %H:%M"
                    ).replace(tzinfo=pytz.timezone("Asia/Shanghai"))

                    # 创建事件
                    event = Event()
                    event.add("summary", f"{course_name} - {teacher}")
                    event.add("description", f"教师: {teacher}\n教室: {location}")
                    event.add("location", location)
                    event.add("dtstart", start_time)
                    event.add("dtend", end_time)
                    event.add("dtstamp", datetime.now(pytz.utc))
                    event.add(
                        "uid",
                        f"{class_date.date()}-{start_time_str}-{course_name}@example.com",
                    )

                    # 添加提醒(提前15分钟)
                    alarm = Alarm()
                    alarm.add("action", "DISPLAY")
                    alarm.add("trigger", timedelta(minutes=-15))
                    event.add_component(alarm)

                    cal.add_component(event)

            except Exception as e:
                print(f"解析课程失败:{course}\n错误信息:{e}")

# 将日历写入文件
output_path = os.path.join(script_dir, "class_schedule_2025.ics")
with open(output_path, "wb") as f:
    f.write(cal.to_ical())

print(f"ICS 文件已生成:{output_path}")
