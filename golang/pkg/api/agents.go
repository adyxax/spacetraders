package api

import (
	"net/url"

	"git.adyxax.org/adyxax/spacetraders/v2/pkg/model"
)

func (c *Client) MyAgent() (*model.Agent, error) {
	uriRef := url.URL{Path: "my/agent"}
	var response model.Agent
	err := c.Send("GET", &uriRef, nil, &response)
	if err != nil {
		return nil, err
	}
	return &response, nil
}
