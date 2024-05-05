package api

type RegisterMessage struct {
	//agent
	//contract
	//faction
	//ship
	Token string `json:"token"`
}

func (c *Client) Register(faction, symbol string) (APIMessage[RegisterMessage, any], error) {
	type RegisterRequest struct {
		Faction string `json:"faction"`
		Symbol  string `json:"symbol"`
	}
	return Send[RegisterMessage](c, "POST", "/register", RegisterRequest{
		Faction: faction,
		Symbol:  symbol,
	})
}
