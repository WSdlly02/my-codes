package main

import "time"

type savedCookies struct {
	Cookies []savedCookie `json:"cookies"`
}

type savedCookie struct {
	Name     string     `json:"name"`
	Value    string     `json:"value"`
	Domain   string     `json:"domain,omitempty"`
	Path     string     `json:"path,omitempty"`
	Expires  *time.Time `json:"expires,omitempty"`
	HttpOnly bool       `json:"httpOnly,omitempty"`
	Secure   bool       `json:"secure,omitempty"`
}

type channelEntry struct {
	RoundNo      string     `json:"roundNo"`
	Name         string     `json:"name"`
	OpenTime     string     `json:"openTime"`
	Notice       string     `json:"notice"`
	ProfileID    string     `json:"profileId"`
	Opened       bool       `json:"opened"`
	BeginAt      *time.Time `json:"beginAt,omitempty"`
	DiscoveredAt time.Time  `json:"discoveredAt"`
}

type channelCache struct {
	FetchedAt time.Time      `json:"fetchedAt"`
	SourceURL string         `json:"sourceUrl"`
	Channels  []channelEntry `json:"channels"`
}

type lesson struct {
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
	ArrangeInfo        []arrangeInfo `json:"arrangeInfo"`
}

type arrangeInfo struct {
	WeekDay   int    `json:"weekDay"`
	WeekState string `json:"weekState"`
	StartUnit int    `json:"startUnit"`
	EndUnit   int    `json:"endUnit"`
	Rooms     string `json:"rooms"`
}

type lessonCount struct {
	Selected int `json:"sc"`
	Limit    int `json:"lc"`
	Reserved int `json:"wc"`
}

type lessonRef struct {
	ID   int    `json:"id"`
	No   string `json:"no"`
	Code string `json:"code"`
	Name string `json:"name"`
}

type lessonMappingCache struct {
	ProfileID  string               `json:"profileId"`
	FetchedAt  time.Time            `json:"fetchedAt"`
	SourceURL  string               `json:"sourceUrl"`
	Lessons    []lesson             `json:"lessons"`
	ByLessonID map[string]lessonRef `json:"byLessonId"`
	ByName     map[string][]string  `json:"byName"`
	ByCode     map[string][]string  `json:"byCode"`
	ByTeacher  map[string][]string  `json:"byTeacher"`
}

type lessonCountSnapshot struct {
	ProfileID string                 `json:"profileId"`
	FetchedAt time.Time              `json:"fetchedAt"`
	SourceURL string                 `json:"sourceUrl"`
	Counts    map[string]lessonCount `json:"counts"`
}
