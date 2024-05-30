package model

type Agent struct {
	AccountID       string `json:"accountId"`
	Credits         int    `json:"credits"`
	Headquarters    string `json:"headquarters"`
	ShipCount       int    `json:"shipCount"`
	StartingFaction string `json:"startingFaction"`
	Symbol          string `json:"symbol"`
}
