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
		return fmt.Errorf("failed API request: %w", err)
	}
	if err := db.SaveAgent(response.Agent); err != nil {
		return fmt.Errorf("failed to save agent: %w", err)
	}
	contract.Accepted = response.Contract.Accepted
	contract.Terms = response.Contract.Terms
	return nil
}

func (c *Client) Deliver(contract *model.Contract, ship *model.Ship, db *database.DB) error {
	deliver := contract.Terms.Deliver[0]
	var units int
	for _, cargoItem := range ship.Cargo.Inventory {
		if cargoItem.Symbol == deliver.TradeSymbol {
			units = min(deliver.UnitsRequired-deliver.UnitsFulfilled, cargoItem.Units)
			break
		}
	}
	uriRef := url.URL{Path: path.Join("my/contracts", contract.Id, "deliver")}
	type deliverRequest struct {
		ShipSymbol  string `json:"shipSymbol"`
		TradeSymbol string `json:"tradeSymbol"`
		Units       int    `json:"units"`
	}
	type deliverResponse struct {
		Cargo    *model.Cargo    `json:"cargo"`
		Contract *model.Contract `json:"contract"`
	}
	var response deliverResponse
	if err := c.Send("POST", &uriRef, deliverRequest{ship.Symbol, deliver.TradeSymbol, units}, &response); err != nil {
		return fmt.Errorf("failed API request: %w", err)
	}
	ship.Cargo = response.Cargo
	contract.Terms = response.Contract.Terms
	return nil
}

func (c *Client) Fulfill(contract *model.Contract, db *database.DB) error {
	if contract.Fulfilled {
		return nil
	}
	uriRef := url.URL{Path: path.Join("my/contracts", contract.Id, "fulfill")}
	type fulfillResponse struct {
		Agent    *model.Agent    `json:"agent"`
		Contract *model.Contract `json:"contract"`
	}
	var response fulfillResponse
	if err := c.Send("POST", &uriRef, nil, &response); err != nil {
		return fmt.Errorf("failed API request: %w", err)
	}
	if err := db.SaveAgent(response.Agent); err != nil {
		return fmt.Errorf("failed to save agent: %w", err)
	}
	contract.Fulfilled = response.Contract.Fulfilled
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
