package api

import (
	"fmt"
	"net/url"
	"path"
	"time"

	"git.adyxax.org/adyxax/spacetraders/golang/pkg/model"
)

func (c *Client) Dock(s *model.Ship) error {
	if s.Nav.Status == "DOCKED" {
		return nil
	}
	uriRef := url.URL{Path: path.Join("my/ships", s.Symbol, "dock")}
	type dockResponse struct {
		Nav *model.Nav `json:"nav"`
	}
	var response dockResponse
	if err := c.Send("POST", &uriRef, nil, &response); err != nil {
		return fmt.Errorf("failed API request: %w", err)
	}
	s.Nav = response.Nav
	return nil
}

func (c *Client) MyShips() ([]model.Ship, error) {
	uriRef := url.URL{Path: "my/ships"}
	var ships []model.Ship
	if err := c.Send("GET", &uriRef, nil, &ships); err != nil {
		return nil, fmt.Errorf("failed API request: %w", err)
	}
	return ships, nil
}

func (c *Client) Navigate(s *model.Ship, waypointSymbol string) error {
	if s.Nav.WaypointSymbol == waypointSymbol {
		return nil
	}
	if err := c.orbit(s); err != nil {
		return fmt.Errorf("failed to orbit: %w", err)
	}
	uriRef := url.URL{Path: path.Join("my/ships", s.Symbol, "navigate")}
	type navigateRequest struct {
		WaypointSymbol string `json:"waypointSymbol"`
	}
	type navigateResponse struct {
		//Events        []model.Event        `json:"events"`
		Fuel *model.Fuel `json:"fuel"`
		Nav  *model.Nav  `json:"nav"`
	}
	var response navigateResponse
	if err := c.Send("POST", &uriRef, navigateRequest{waypointSymbol}, &response); err != nil {
		return fmt.Errorf("failed API request: %w", err)
	}
	s.Fuel = response.Fuel
	s.Nav = response.Nav
	select {
	case <-c.ctx.Done():
		return fmt.Errorf("failed: context cancelled")
	case <-time.After(time.Until(s.Nav.Route.Arrival)):
	}
	s.Nav.Status = "IN_ORBIT"
	return nil
}

func (c *Client) NegotiateContract(s *model.Ship) (*model.Contract, error) {
	uriRef := url.URL{Path: path.Join("my/ships", s.Symbol, "negotiate", "contract")}
	type negotiateResponse struct {
		Contract *model.Contract `json:"contract"`
	}
	var response negotiateResponse
	if err := c.Send("POST", &uriRef, nil, &response); err != nil {
		return nil, fmt.Errorf("failed API request: %w", err)
	}
	return response.Contract, nil
}

func (c *Client) orbit(s *model.Ship) error {
	if s.Nav.Status == "IN_ORBIT" {
		return nil
	}
	uriRef := url.URL{Path: path.Join("my/ships", s.Symbol, "orbit")}
	type orbitResponse struct {
		Nav *model.Nav `json:"nav"`
	}
	var response orbitResponse
	if err := c.Send("POST", &uriRef, nil, &response); err != nil {
		return fmt.Errorf("failed API request: %w", err)
	}
	s.Nav = response.Nav
	return nil
}

func (c *Client) Purchase(s *model.Ship, cargoItem string, units int) error {
	if err := c.Dock(s); err != nil {
		return fmt.Errorf("failed to dock: %w", err)
	}
	uriRef := url.URL{Path: path.Join("my/ships", s.Symbol, "purchase")}
	type purchaseRequest struct {
		Symbol string `json:"symbol"`
		Units  int    `json:"units"`
	}
	type purchaseResponse struct {
		Agent       *model.Agent       `json:"agent"`
		Cargo       *model.Cargo       `json:"cargo"`
		Transaction *model.Transaction `json:"transaction"`
	}
	var response purchaseResponse
	if err := c.Send("POST", &uriRef, purchaseRequest{cargoItem, units}, &response); err != nil {
		return fmt.Errorf("failed API request: %w", err)
	}
	if err := c.db.SaveAgent(response.Agent); err != nil {
		return fmt.Errorf("failed to save agent: %w", err)
	}
	s.Cargo = response.Cargo
	if err := c.db.AppendTransaction(response.Transaction); err != nil {
		return fmt.Errorf("failed to append transaction: %w", err)
	}
	return nil
}

func (c *Client) Refuel(s *model.Ship) error {
	if s.Fuel.Current == s.Fuel.Capacity {
		return nil
	}
	if err := c.Dock(s); err != nil {
		return fmt.Errorf("failed to dock: %w", err)
	}
	uriRef := url.URL{Path: path.Join("my/ships", s.Symbol, "refuel")}
	type refuelResponse struct {
		Agent       *model.Agent       `json:"agent"`
		Fuel        *model.Fuel        `json:"fuel"`
		Transaction *model.Transaction `json:"transaction"`
	}
	var response refuelResponse
	if err := c.Send("POST", &uriRef, nil, &response); err != nil {
		return fmt.Errorf("failed API request: %w", err)
	}
	if err := c.db.SaveAgent(response.Agent); err != nil {
		return fmt.Errorf("failed to save agent: %w", err)
	}
	s.Fuel = response.Fuel
	if err := c.db.AppendTransaction(response.Transaction); err != nil {
		return fmt.Errorf("failed to append transaction: %w", err)
	}
	return nil
}

func (c *Client) Sell(s *model.Ship, cargoItem string, units int) error {
	if err := c.Dock(s); err != nil {
		return fmt.Errorf("failed to dock: %w", err)
	}
	uriRef := url.URL{Path: path.Join("my/ships", s.Symbol, "sell")}
	type sellRequest struct {
		Symbol string `json:"symbol"`
		Units  int    `json:"units"`
	}
	type sellResponse struct {
		Agent       *model.Agent       `json:"agent"`
		Cargo       *model.Cargo       `json:"cargo"`
		Transaction *model.Transaction `json:"transaction"`
	}
	var response sellResponse
	if err := c.Send("POST", &uriRef, sellRequest{cargoItem, units}, &response); err != nil {
		return fmt.Errorf("failed API request: %w", err)
	}
	if err := c.db.SaveAgent(response.Agent); err != nil {
		return fmt.Errorf("failed to save agent: %w", err)
	}
	s.Cargo = response.Cargo
	if err := c.db.AppendTransaction(response.Transaction); err != nil {
		return fmt.Errorf("failed to append transaction: %w", err)
	}
	return nil
}
