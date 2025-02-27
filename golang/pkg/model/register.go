package model

type Register struct {
	Agent *Agent `json:"agent"`
	//contract
	//faction
	Ships []Ship `json:"ships"`
	Token string `json:"token"`
}
