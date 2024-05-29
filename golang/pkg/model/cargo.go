package model

type Cargo struct {
	Capacity  int         `json:"capacity"`
	Inventory []Inventory `json:"inventory"`
	Units     int         `json:"units"`
}
