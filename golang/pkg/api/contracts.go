package api

import (
	"fmt"
	"net/url"

	"git.adyxax.org/adyxax/spacetraders/golang/pkg/model"
)

func (c *Client) MyContracts() ([]model.Contract, error) {
	uriRef := url.URL{Path: "my/contracts"}
	var contracts []model.Contract
	if err := c.Send("GET", &uriRef, nil, &contracts); err != nil {
		return nil, fmt.Errorf("failed to get contracts: %w", err)
	}
	return contracts, nil
}
