package model

type TradeGood struct {
	Activity      string `json:"activity"`
	PurchasePrice int    `json:"purchasePrice"`
	SellPrice     int    `json:"sellPrice"`
	Supply        string `json:"supply"`
	Symbol        string `json:"symbol"`
	TradeVolume   int    `json:"tradeVolume"`
	Type          string `json:"type"`
}
