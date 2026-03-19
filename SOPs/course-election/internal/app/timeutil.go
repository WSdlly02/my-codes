package app

import "time"

const timeLayout = "2006-01-02 15:04:05 MST"

func localTZ() *time.Location {
	loc, err := time.LoadLocation("Asia/Shanghai")
	if err != nil {
		return time.Local
	}
	return loc
}

func now() time.Time {
	return time.Now().In(localTZ())
}
