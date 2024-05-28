package api

import (
	"encoding/json"
	"fmt"
	"net/url"
)

type AgentMessage struct {
	AccountID       string `json:"accountId"`
	Credits         int    `json:"credits"`
	Headquarters    string `json:"headquarters"`
	ShipCount       int    `json:"shipCount"`
	StartingFaction string `json:"startingFaction"`
	Symbol          string `json:"symbol"`
}

func (c *Client) MyAgent() (*AgentMessage, error) {
	uriRef := url.URL{Path: "my/agent"}
	msg, err := c.Send("GET", &uriRef, nil)
	if err != nil {
		return nil, err
	}
	if msg.Error != nil {
		return nil, msg.Error
	}
	var response AgentMessage
	if err := json.Unmarshal(msg.Data, &response); err != nil {
		return nil, fmt.Errorf("failed to unmarshal agent data: %w", err)
	}
	return &response, nil
}
