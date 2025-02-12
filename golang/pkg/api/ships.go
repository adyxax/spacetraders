package api

import (
	"fmt"
	"net/url"
	"path"

	"git.adyxax.org/adyxax/spacetraders/golang/pkg/model"
)

func (c *Client) Dock(s *model.Ship) error {
	if s.Nav.Status == "DOCKED" {
		return nil
	}
	type DockResponse struct {
		Nav model.Nav `json:"nav"`
	}
	uriRef := url.URL{Path: path.Join("my/ships", s.Symbol, "dock")}
	var response DockResponse
	err := c.Send("POST", &uriRef, nil, &response)
	if err != nil {
		return fmt.Errorf("failed to dock ship %s: %w", s.Symbol, err)
	}
	s.Nav = response.Nav
	return nil
}

func (c *Client) MyShips() ([]model.Ship, error) {
	uriRef := url.URL{Path: "my/ships"}
	var response []model.Ship
	err := c.Send("GET", &uriRef, nil, &response)
	if err != nil {
		return nil, fmt.Errorf("failed to get ships: %w", err)
	}
	return response, nil
}

func (c *Client) Orbit(s *model.Ship) error {
	if s.Nav.Status == "IN_ORBIT" {
		return nil
	}
	type OrbitResponse struct {
		Nav model.Nav `json:"nav"`
	}
	uriRef := url.URL{Path: path.Join("my/ships", s.Symbol, "orbit")}
	var response OrbitResponse
	err := c.Send("POST", &uriRef, nil, &response)
	if err != nil {
		return fmt.Errorf("failed to orbit ship %s: %w", s.Symbol, err)
	}
	s.Nav = response.Nav
	return nil
}
