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
