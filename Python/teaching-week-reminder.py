from icalendar import Calendar, Event, Alarm
from datetime import datetime, timedelta
import pytz
import os

# 第一个教学周的周一日期(2025 年 2 月 17 日 8:00，中国时区)
start_date = datetime(2025, 2, 17, 8, 0, tzinfo=pytz.timezone("Asia/Shanghai"))

# 创建日历对象
cal = Calendar()
cal.add("prodid", "-//Teaching Weeks Reminder 2025//example.com//")
cal.add("version", "2.0")

# 生成 17 周的周一提醒
for week in range(1, 18):
    # 计算当前周的周一日期
    current_date = start_date + timedelta(weeks=week - 1)

    # 创建事件(持续 30 分钟，仅用于占位)
    event = Event()
    event.add("summary", f"教学周提醒-第{week}周")
    event.add("dtstart", current_date)
    event.add("dtend", current_date + timedelta(minutes=30))
    event.add("dtstamp", datetime.now(pytz.utc))
    event.add("uid", f"teaching-week-{week}-2025@example.com")  # 唯一标识符

    # 不用提醒
    # alarm = Alarm()
    # alarm.add("action", "DISPLAY")
    # alarm.add("trigger", timedelta(minutes=-15))  # 提前 15 分钟
    # event.add_component(alarm)

    cal.add_component(event)

# 将日历写入文件
script_dir = "/home/wsdlly02/Documents/my-codes/Static"
output_path = os.path.join(script_dir, "teaching_week_reminder_2025.ics")
with open(output_path, "wb") as f:
    f.write(cal.to_ical())

print(f"教学周提示日历已生成:{output_path}")
