package model

type Cargo struct {
	Capacity  int             `json:"capacity"`
	Inventory []InventoryItem `json:"inventory"`
	Units     int             `json:"units"`
}
