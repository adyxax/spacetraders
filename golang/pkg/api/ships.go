package api

import (
	"encoding/json"
	"fmt"
	"net/url"
	"path"
	"time"

	"git.adyxax.org/adyxax/spacetraders/v2/pkg/model"
)

func (c *Client) Dock(s *model.Ship) error {
	if s.Nav.Status == "DOCKED" {
		return nil
	}
	type DockResponse struct {
		Nav model.Nav `json:"nav"`
	}
	uriRef := url.URL{Path: path.Join("my/ships", s.Symbol, "dock")}
	msg, err := c.Send("POST", &uriRef, nil)
	if err != nil {
		return fmt.Errorf("failed to dock ship %s: %w", s.Symbol, err)
	}
	if msg.Error != nil {
		switch msg.Error.Code {
		case 4214:
			e := decodeShipInTransitError(msg.Error.Data)
			time.Sleep(e.SecondsToArrival.Duration() * time.Second)
			return c.Dock(s)
		default:
			return msg.Error
		}
	}
	var response DockResponse
	if err := json.Unmarshal(msg.Data, &response); err != nil {
		return fmt.Errorf("failed to unmarshal dock data: %w", err)
	}
	s.Nav = response.Nav
	return nil
}

func (c *Client) MyShips() ([]model.Ship, error) {
	uriRef := url.URL{Path: "my/ships"}
	msg, err := c.Send("GET", &uriRef, nil)
	if err != nil {
		return nil, fmt.Errorf("failed to get ships: %w", err)
	}
	if msg.Error != nil {
		return nil, fmt.Errorf("failed to get ships: %w", msg.Error)
	}
	var response []model.Ship
	if err := json.Unmarshal(msg.Data, &response); err != nil {
		return nil, fmt.Errorf("failed to unmarshal ships data: %w", err)
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
	msg, err := c.Send("POST", &uriRef, nil)
	if err != nil {
		return fmt.Errorf("failed to orbit ship %s: %w", s.Symbol, err)
	}
	if msg.Error != nil {
		switch msg.Error.Code {
		case 4214:
			e := decodeShipInTransitError(msg.Error.Data)
			time.Sleep(e.SecondsToArrival.Duration() * time.Second)
			return c.Orbit(s)
		default:
			return msg.Error
		}
	}
	var response OrbitResponse
	if err := json.Unmarshal(msg.Data, &response); err != nil {
		return fmt.Errorf("failed to unmarshal orbit data: %w", err)
	}
	s.Nav = response.Nav
	return nil
}
