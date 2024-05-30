package model

type Register struct {
	Agent Agent `json:"agent"`
	//contract
	//faction
	Ship  Ship   `json:"ship"`
	Token string `json:"token"`
}
