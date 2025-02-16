package model

import "time"

type Shipyard struct {
	ModificationFee int                   `json:"modificationFee"`
	Symbol          string                `json:"symbol"`
	ShipTypes       []CommonType          `json:"shipTypes"`
	Transactions    []ShipyardTransaction `json:"transactions"`
	Ships           []ShipyardShip        `json:"ships"`
}

type ShipyardShip struct {
	Activity string `json:"activity"`
	// crew
	//Description string `json:"description"`
	// engine
	// frame
	// modules
	// mounts
	//Name         string   `json:"name"`
	PurchasePrice int `json:"purchasePrice"`
	// reactor
	Supply string `json:"supply"`
	Type   string `json:"type"`
}

type ShipyardTransaction struct {
	AgentSymbol    string    `json:"agentSymbol"`
	Price          int       `json:"price"`
	ShipSymbol     string    `json:"shipSymbol"`
	ShipType       string    `json:"shipType"`
	Timestamp      time.Time `json:"timestamp"`
	WaypointSymbol string    `json:"waypointSymbol"`
}
