package model

import "time"

type Contract struct {
	Accepted         bool      `json:"accepted"`
	DeadlineToAccept time.Time `json:"deadlineToAccept"`
	Expiration       time.Time `json:"expiration"`
	FactionSymbol    string    `json:"factionSymbol"`
	Fullfilled       bool      `json:"fulfilled"`
	Id               string    `json:"id"`
	Terms            *Terms    `json:"terms"`
	Type             string    `json:"type"`
}

type Deliver struct {
	DestinationSymbol string `json:"destinationSymbol"`
	TradeSymbol       string `json:"tradeSymbol"`
	UnitsFulfilled    int    `json:"unitsFulfilled"`
	UnitsRequired     int    `json:"unitsRequired"`
}

type Payment struct {
	OnAccepted  int `json:"onAccepted"`
	OnFulfilled int `json:"onFulfilled"`
}

type Terms struct {
	Deadline time.Time `json:"deadline"`
	Payment  *Payment  `json:"payment"`
	Deliver  []Deliver `json:"deliver"`
}
