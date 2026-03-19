# go run . query --code "FX120230"
此时的mapping_2936.json 的原始数据：
```json
[
  {
    "id": 242153,
    "no": "FX120230_002",
    "name": "海事法",
    "code": "FX120230",
    "credits": 2,
    "courseId": 172,
    "startWeek": 1,
    "endWeek": 16,
    "courseTypeId": 2,
    "courseTypeName": "&nbsp;",
    "courseTypeCode": "0005",
    "courseCategoryName": "一般课程",
    "teachDepartName": "法学院",
    "examModeName": "考查",
    "scheduled": true,
    "hasTextBook": false,
    "period": 32,
    "weekHour": 2,
    "withdrawable": true,
    "textbooks": "",
    "teachers": "张一祯",
    "campusCode": "1",
    "langType": "中文",
    "campusName": "临港校区",
    "adminClass": "国航231 国航232 国航233 国航234 新国航231",
    "remark": "",
    "arrangeInfo": [
      {
        "weekDay": 2,
        "weekState": "01111111111111111000000000000000000000000000000000000",
        "startUnit": 7,
        "endUnit": 8,
        "rooms": "教学3B201"
      }
    ]
  },
  {
    "id": 242095,
    "no": "FX120230_001",
    "name": "海事法",
    "code": "FX120230",
    "credits": 2,
    "courseId": 172,
    "startWeek": 1,
    "endWeek": 16,
    "courseTypeId": 2,
    "courseTypeName": "&nbsp;",
    "courseTypeCode": "0005",
    "courseCategoryName": "一般课程",
    "teachDepartName": "法学院",
    "examModeName": "考查",
    "scheduled": true,
    "hasTextBook": false,
    "period": 32,
    "weekHour": 2,
    "withdrawable": true,
    "textbooks": "",
    "teachers": "李雯雯",
    "campusCode": "1",
    "langType": "中文",
    "campusName": "临港校区",
    "adminClass": "交运国际231 交运231 交运232 英航231 英航232",
    "remark": "",
    "arrangeInfo": [
      {
        "weekDay": 4,
        "weekState": "00000000000000011000000000000000000000000000000000000",
        "startUnit": 11,
        "endUnit": 12,
        "rooms": "教学2A103"
      },
      {
        "weekDay": 5,
        "weekState": "01111100111111111000000000000000000000000000000000000",
        "startUnit": 9,
        "endUnit": 10,
        "rooms": "教学3B303"
      }
    ]
  }
]
```

# 实际输出结果：
```
[查询结果1]
课程ID: 242153
课程号: FX120230
课序号: FX120230_002
课程名称: 海事法
学分: 2
课程类型: 一般课程
开课院系: 法学院
考试方式: 考查
授课教师: 张一祯
学时: 32
行政班: 国航231 国航232 国航233 国航234 新国航231
上课信息1: 周二 7-8节 1-16周 教学3B201

[查询结果2]
课程ID: 242095
课程号: FX120230
课序号: FX120230_001
课程名称: 海事法
学分: 2
课程类型: 一般课程
开课院系: 法学院
考试方式: 考查
授课教师: 李雯雯
学时: 32
行政班: 交运国际231 交运231 交运232 英航231 英航232
上课信息1: 周四 11-12节 15-16周 教学2A103
上课信息2: 周五 9-10节 1-5 8-16周 教学3B303
```

## 除了上课信息之外的信息都可以简单获取
## 上课信息的获取需要对arrangeInfo进行处理
- weekDay: 1-7分别对应周一到周日
- startUnit和endUnit分别对应课程的开始节数和结束节数
- weekState是一个长度为53的字符串，每个字符代表一周,从0开始（第0周），'1'表示该周有课，'0'表示该周无课
- rooms是上课地点