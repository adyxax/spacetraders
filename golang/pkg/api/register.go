package api

import (
	"encoding/json"
	"fmt"
	"net/url"
)

type RegisterMessage struct {
	//agent
	//contract
	//faction
	//ship
	Token string `json:"token"`
}

func (c *Client) Register(faction, symbol string) (*RegisterMessage, error) {
	type RegisterRequest struct {
		Faction string `json:"faction"`
		Symbol  string `json:"symbol"`
	}
	uriRef := url.URL{Path: "register"}
	msg, err := c.Send("POST", &uriRef, RegisterRequest{
		Faction: faction,
		Symbol:  symbol,
	})
	if err != nil {
		return nil, err
	}
	if msg.Error != nil {
		return nil, msg.Error
	}
	var response RegisterMessage
	if err := json.Unmarshal(msg.Data, &response); err != nil {
		return nil, fmt.Errorf("failed to unmarshal register data: %w", err)
	}
	return &response, nil
}
