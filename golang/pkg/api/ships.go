package api

import (
	"fmt"
	"net/url"
	"path"
	"time"

	"git.adyxax.org/adyxax/spacetraders/golang/pkg/database"
	"git.adyxax.org/adyxax/spacetraders/golang/pkg/model"
)

func (c *Client) dock(s *model.Ship) error {
	if s.Nav.Status == "DOCKED" {
		return nil
	}
	uriRef := url.URL{Path: path.Join("my/ships", s.Symbol, "dock")}
	type dockResponse struct {
		Nav *model.Nav `json:"nav"`
	}
	var response dockResponse
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

func (c *Client) Navigate(s *model.Ship, w *model.Waypoint, db *database.DB) error {
	// TODO shortest path
	// TODO go refuel if necessary
	if err := c.orbit(s); err != nil {
		return fmt.Errorf("failed to navigate ship %s to %s: %w", s.Symbol, w.Symbol, err)
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
	if err := c.Send("POST", &uriRef, navigateRequest{w.Symbol}, &response); err != nil {
		return fmt.Errorf("failed to navigate ship %s to %s: %w", s.Symbol, w.Symbol, err)
	}
	s.Fuel = response.Fuel
	s.Nav = response.Nav
	select {
	case <-c.ctx.Done():
		return fmt.Errorf("failed to navigate ship %s to %s: ctx cancelled", s.Symbol, w.Symbol)
	case <-time.After(s.Nav.Route.Arrival.Sub(time.Now())):
	}
	s.Nav.Status = "IN_ORBIT"
	return nil
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
		return fmt.Errorf("failed to orbit ship %s: %w", s.Symbol, err)
	}
	s.Nav = response.Nav
	return nil
}

func (c *Client) refuel(s *model.Ship, db *database.DB) error {
	if s.Fuel.Current == s.Fuel.Capacity {
		return nil
	}
	if err := c.dock(s); err != nil {
		return fmt.Errorf("failed to refuel ship %s: %w", s.Symbol, err)
	}
	uriRef := url.URL{Path: path.Join("my/ships", s.Symbol, "refuel")}
	type refuelResponse struct {
		Agent       *model.Agent       `json:"agent"`
		Fuel        *model.Fuel        `json:"fuel"`
		Transaction *model.Transaction `json:"transaction"`
	}
	var response refuelResponse
	if err := c.Send("POST", &uriRef, nil, &response); err != nil {
		return fmt.Errorf("failed to refuel ship %s: %w", s.Symbol, err)
	}
	if err := db.SaveAgent(response.Agent); err != nil {
		return fmt.Errorf("failed to refuel ship %s: %w", s.Symbol, err)
	}
	s.Fuel = response.Fuel
	if err := db.AppendTransaction(response.Transaction); err != nil {
		return fmt.Errorf("failed to refuel ship %s: %w", s.Symbol, err)
	}
	return nil
}
