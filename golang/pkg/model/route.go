package model

import "time"

type Route struct {
	Arrival       time.Time     `json:"arrival"`
	DepartureTime time.Time     `json:"departureTime"`
	Destination   RouteEndpoint `json:"destination"`
}

type RouteEndpoint struct {
	Type         string `json:"type"`
	Symbol       string `json:"symbol"`
	SystemSymbol string `json:"systemSymbol"`
	X            int    `json:"x"`
	Y            int    `json:"y"`
}
