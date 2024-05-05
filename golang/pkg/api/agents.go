package api

type AgentMessage struct {
	AccountID       string `json:"accountId"`
	Credits         int    `json:"credits"`
	Headquarters    string `json:"headquarters"`
	ShipCount       int    `json:"shipCount"`
	StartingFaction string `json:"startingFaction"`
	Symbol          string `json:"symbol"`
}

func (c *Client) MyAgent() (APIMessage[AgentMessage, any], error) {
	return Send[AgentMessage](c, "GET", "/my/agent", nil)
}
