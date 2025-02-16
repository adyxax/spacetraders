package model

type Waypoint struct {
	Chart               *Chart   `json:"chart"`
	Factions            []string `json:"factions"`
	IsUnderConstruction bool     `json:"isUnderConstruction"`
	Modifiers           []Common `json:"modifiers"`
	//orbitals: Array<{symbol: string;}>;
	//orbits: string;
	Symbol       string   `json:"symbol"`
	SystemSymbol string   `json:"systemSymbol"`
	Traits       []Common `json:"traits"`
	Type         string   `json:"type"`
	X            int      `json:"x"`
	Y            int      `json:"y"`
}

func (w Waypoint) GetX() int {
	return w.X
}

func (w Waypoint) GetY() int {
	return w.Y
}
