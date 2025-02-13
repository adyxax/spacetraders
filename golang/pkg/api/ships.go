package api

import (
	"fmt"
	"net/url"
	"path"

	"git.adyxax.org/adyxax/spacetraders/golang/pkg/agent"
	"git.adyxax.org/adyxax/spacetraders/golang/pkg/database"
	"git.adyxax.org/adyxax/spacetraders/golang/pkg/model"
)

func (c *Client) Dock(s *model.Ship) error {
	if s.Nav.Status == "DOCKED" {
		return nil
	}
	uriRef := url.URL{Path: path.Join("my/ships", s.Symbol, "dock")}
	type DockResponse struct {
		Nav *model.Nav `json:"nav"`
	}
	var response DockResponse
	if err := c.Send("POST", &uriRef, nil, &response); err != nil {
		return fmt.Errorf("failed to dock ship %s: %w", s.Symbol, err)
	}
	s.Nav = response.Nav
	return nil
}

func (c *Client) MyShips() ([]model.Ship, error) {
	uriRef := url.URL{Path: "my/ships"}
	var ships []model.Ship
	if err := c.Send("GET", &uriRef, nil, &ships); err != nil {
		return nil, fmt.Errorf("failed to get ships: %w", err)
	}
	return ships, nil
}

func (c *Client) Orbit(s *model.Ship) error {
	if s.Nav.Status == "IN_ORBIT" {
		return nil
	}
	uriRef := url.URL{Path: path.Join("my/ships", s.Symbol, "orbit")}
	type OrbitResponse struct {
		Nav *model.Nav `json:"nav"`
	}
	var response OrbitResponse
	if err := c.Send("POST", &uriRef, nil, &response); err != nil {
		return fmt.Errorf("failed to orbit ship %s: %w", s.Symbol, err)
	}
	s.Nav = response.Nav
	return nil
}

func (c *Client) Refuel(s *model.Ship, db *database.DB) error {
	if s.Fuel.Current == s.Fuel.Capacity {
		return nil
	}
	if err := c.Dock(s); err != nil {
		return fmt.Errorf("failed to refuel ship %s: %w", s.Symbol, err)
	}
	uriRef := url.URL{Path: path.Join("my/ships", s.Symbol, "refuel")}
	type RefuelResponse struct {
		Agent       *model.Agent       `json:"agent"`
		Fuel        *model.Fuel        `json:"fuel"`
		Transaction *model.Transaction `json:"transaction"`
	}
	var response RefuelResponse
	if err := c.Send("POST", &uriRef, nil, &response); err != nil {
		return fmt.Errorf("failed to refuel ship %s: %w", s.Symbol, err)
	}
	agent.SetAgent(response.Agent)
	s.Fuel = response.Fuel
	if err := db.AppendTransaction(response.Transaction); err != nil {
		return fmt.Errorf("failed to refuel ship %s: %w", s.Symbol, err)
	}
	return nil
}
