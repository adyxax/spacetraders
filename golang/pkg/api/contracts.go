package api

import (
	"fmt"
	"net/url"
	"path"

	"git.adyxax.org/adyxax/spacetraders/golang/pkg/database"
	"git.adyxax.org/adyxax/spacetraders/golang/pkg/model"
)

func (c *Client) Accept(contract *model.Contract, db *database.DB) error {
	if contract.Accepted {
		return nil
	}
	uriRef := url.URL{Path: path.Join("my/contracts", contract.Id, "accept")}
	type acceptResponse struct {
		Agent    *model.Agent    `json:"agent"`
		Contract *model.Contract `json:"contract"`
	}
	var response acceptResponse
	if err := c.Send("POST", &uriRef, nil, &response); err != nil {
		return fmt.Errorf("failed to accept contract %s: %w", contract.Id, err)
	}
	if err := db.SaveAgent(response.Agent); err != nil {
		return fmt.Errorf("failed to accept contract %s: %w", contract.Id, err)
	}
	contract.Accepted = response.Contract.Accepted
	contract.Terms = response.Contract.Terms
	return nil
}

func (c *Client) MyContracts() ([]model.Contract, error) {
	uriRef := url.URL{Path: "my/contracts"}
	var contracts []model.Contract
	if err := c.Send("GET", &uriRef, nil, &contracts); err != nil {
		return nil, fmt.Errorf("failed API request: %w", err)
	}
	return contracts, nil
}
