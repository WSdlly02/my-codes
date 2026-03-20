package main

// 课程信息结构体
type LessonMappingCache struct {
	ProfileID string   `json:"profileId"`
	Lessons   []Lesson `json:"lessons"`
}

type Lesson struct {
	ID                 int           `json:"id"`
	No                 string        `json:"no"`
	Name               string        `json:"name"`
	Code               string        `json:"code"`
	Credits            float64       `json:"credits"`
	CourseID           int           `json:"courseId"`
	StartWeek          int           `json:"startWeek"`
	EndWeek            int           `json:"endWeek"`
	CourseTypeID       int           `json:"courseTypeId"`
	CourseTypeName     string        `json:"courseTypeName"`
	CourseTypeCode     string        `json:"courseTypeCode"`
	CourseCategoryName string        `json:"courseCategoryName"`
	TeachDepartName    string        `json:"teachDepartName"`
	ExamModeName       string        `json:"examModeName"`
	Scheduled          bool          `json:"scheduled"`
	HasTextBook        bool          `json:"hasTextBook"`
	Period             int           `json:"period"`
	WeekHour           int           `json:"weekHour"`
	Withdrawable       bool          `json:"withdrawable"`
	Textbooks          string        `json:"textbooks"`
	Teachers           string        `json:"teachers"`
	CampusCode         string        `json:"campusCode"`
	LangType           string        `json:"langType"`
	CampusName         string        `json:"campusName"`
	AdminClass         string        `json:"adminClass"`
	Remark             string        `json:"remark"`
	ArrangeInfo        []ArrangeInfo `json:"arrangeInfo"`
}

type ArrangeInfo struct {
	WeekDay   int    `json:"weekDay"`
	WeekState string `json:"weekState"`
	StartUnit int    `json:"startUnit"`
	EndUnit   int    `json:"endUnit"`
	Rooms     string `json:"rooms"`
}

var UnitToTimes = map[int][2]string{
	1:  {"08:20", "09:05"},
	2:  {"09:10", "09:55"},
	3:  {"10:15", "11:00"},
	4:  {"11:05", "11:50"},
	5:  {"11:55", "12:25"},
	6:  {"12:30", "13:00"},
	7:  {"13:10", "13:55"},
	8:  {"14:00", "14:45"},
	9:  {"15:05", "15:50"},
	10: {"15:55", "16:40"},
	11: {"18:00", "18:45"},
	12: {"18:50", "19:35"},
	13: {"19:40", "20:25"},
}
