package model

import "time"

type Transaction struct {
	PricePerUnit   int       `json:"pricePerUnit"`
	ShipSymbol     string    `json:"shipSymbol"`
	Timestamp      time.Time `json:"timestamp"`
	TotalPrice     int       `json:"totalPrice"`
	TradeSymbol    string    `json:"tradeSymbol"`
	Type           string    `json:"type"`
	Units          int       `json:"units"`
	WaypointSymbol string    `json:"waypointSymbol"`
}
