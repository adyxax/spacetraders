package model

type System struct {
	Factions     []Faction  `json:"factions"`
	SectorSymbol string     `json:"sectorSymbol"`
	Symbol       string     `json:"symbol"`
	Type         string     `json:"type"`
	Waypoints    []Waypoint `json:"waypoints"`
	X            int        `json:"x"`
	Y            int        `json:"y"`
}
