package model

type Ship struct {
	Cargo    *Cargo    `json:"cargo"`
	Cooldown *Cooldown `json:"cooldown"`
	// crew
	// engine
	// frame
	Fuel *Fuel `json:"fuel"`
	// modules
	// mounts
	Nav *Nav `json:"nav"`
	// reactor
	//registration: Registration;
	Symbol string `json:"symbol"`
}
