package api

import (
	"fmt"
	"net/url"
	"path"

	"git.adyxax.org/adyxax/spacetraders/golang/pkg/model"
)

func (c *Client) GetMarket(waypointSymbol string, ships []model.Ship) (*model.Market, error) {
	if !isThereAShipDockerAtWaypoint(waypointSymbol, ships) {
		if market, err := c.db.LoadMarket(waypointSymbol); err == nil && market != nil {
			// TODO check last updated time
			return market, nil
		}
	}
	systemSymbol := WaypointSymbolToSystemSymbol(waypointSymbol)
	uriRef := url.URL{Path: path.Join("systems", systemSymbol, "waypoints", waypointSymbol, "market")}
	var market model.Market
	if err := c.Send("GET", &uriRef, nil, &market); err != nil {
		return nil, fmt.Errorf("failed API request: %w", err)
	}
	if err := c.db.SaveMarket(&market); err != nil {
		return nil, fmt.Errorf("failed to save market %s: %w", market.Symbol, err)
	}
	return &market, nil
}

func (c *Client) GetShipyard(waypointSymbol string, ships []model.Ship) (*model.Shipyard, error) {
	if !isThereAShipDockerAtWaypoint(waypointSymbol, ships) {
		if shipyard, err := c.db.LoadShipyard(waypointSymbol); err == nil && shipyard != nil {
			// TODO check last updated time
			return shipyard, nil
		}
	}
	systemSymbol := WaypointSymbolToSystemSymbol(waypointSymbol)
	uriRef := url.URL{Path: path.Join("systems", systemSymbol, "waypoints", waypointSymbol, "shipyard")}
	var shipyard model.Shipyard
	if err := c.Send("GET", &uriRef, nil, &shipyard); err != nil {
		return nil, fmt.Errorf("failed API request: %w", err)
	}
	if err := c.db.SaveShipyard(&shipyard); err != nil {
		return nil, fmt.Errorf("failed to save shipyard %s: %w", shipyard.Symbol, err)
	}
	return &shipyard, nil
}

func (c *Client) GetSystem(systemSymbol string) (*model.System, error) {
	if system, err := c.db.LoadSystem(systemSymbol); err == nil && system != nil {
		return system, nil
	}
	uriRef := url.URL{Path: path.Join("systems", systemSymbol)}
	var system model.System
	if err := c.Send("GET", &uriRef, nil, &system); err != nil {
		return nil, fmt.Errorf("failed API request: %w", err)
	}
	if err := c.db.SaveSystem(&system); err != nil {
		return nil, fmt.Errorf("failed to save system %s: %w", system.Symbol, err)
	}
	return &system, nil
}

func (c *Client) GetWaypoint(waypointSymbol string) (*model.Waypoint, error) {
	if waypoint, err := c.db.LoadWaypoint(waypointSymbol); err == nil && waypoint != nil {
		// TODO check last updated time
		return waypoint, nil
	}
	systemSymbol := WaypointSymbolToSystemSymbol(waypointSymbol)
	uriRef := url.URL{Path: path.Join("systems", systemSymbol, "waypoints", waypointSymbol)}
	var waypoint model.Waypoint
	if err := c.Send("GET", &uriRef, nil, &waypoint); err != nil {
		return nil, fmt.Errorf("failed API request: %w", err)
	}
	if err := c.db.SaveWaypoint(&waypoint); err != nil {
		return nil, fmt.Errorf("failed to save waypoint %s: %w", waypoint.Symbol, err)
	}
	return &waypoint, nil
}

func (c *Client) ListWaypointsInSystem(systemSymbol string) ([]model.Waypoint, error) {
	if waypoints, err := c.db.LoadWaypointsInSystem(systemSymbol); err == nil && len(waypoints) > 0 {
		// TODO check last updated time
		return waypoints, nil
	}
	uriRef := url.URL{Path: path.Join("systems", systemSymbol, "waypoints")}
	var waypoints []model.Waypoint
	if err := c.Send("GET", &uriRef, nil, &waypoints); err != nil {
		return nil, fmt.Errorf("failed API request: %w", err)
	}
	for _, waypoint := range waypoints {
		if err := c.db.SaveWaypoint(&waypoint); err != nil {
			return nil, fmt.Errorf("failed to save waypoint %s: %w", waypoint.Symbol, err)
		}
	}
	return waypoints, nil
}
