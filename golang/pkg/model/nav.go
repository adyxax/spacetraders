package model

type Nav struct {
	FlightMode     string `json:"flightMode"`
	Route          Route  `json:"route"`
	Status         string `json:"status"`
	SystemSymbol   string `json:"systemSymbol"`
	WaypointSymbol string `json:"waypointSymbol"`
}
