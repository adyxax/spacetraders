package api

import (
	"net/url"

	"git.adyxax.org/adyxax/spacetraders/v2/pkg/model"
)

func (c *Client) Register(faction, symbol string) (*model.Register, error) {
	type RegisterRequest struct {
		Faction string `json:"faction"`
		Symbol  string `json:"symbol"`
	}
	uriRef := url.URL{Path: "register"}
	payload := RegisterRequest{
		Faction: faction,
		Symbol:  symbol,
	}
	var response model.Register
	err := c.Send("POST", &uriRef, payload, &response)
	if err != nil {
		return nil, err
	}
	return &response, nil
}
