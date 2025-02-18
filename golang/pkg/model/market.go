package model

type Market struct {
	Exchange     []Common      `json:"exchange"`
	Exports      []Common      `json:"exports"`
	Imports      []Common      `json:"imports"`
	Symbol       string        `json:"symbol"`
	TradeGoods   []TradeGood   `json:"tradeGoods"`
	Transactions []Transaction `json:"transactions"`
}
