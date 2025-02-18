package api

import (
	"fmt"
	"net/url"
	"path"

	"git.adyxax.org/adyxax/spacetraders/golang/pkg/database"
	"git.adyxax.org/adyxax/spacetraders/golang/pkg/model"
)

func (c *Client) GetMarket(waypointSymbol string, db *database.DB) (*model.Market, error) {
	if market, err := db.LoadMarket(waypointSymbol); err == nil && market != nil {
		// TODO check last updated time
		return market, nil
	}
	systemSymbol := WaypointSymbolToSystemSymbol(waypointSymbol)
	uriRef := url.URL{Path: path.Join("systems", systemSymbol, "waypoints", waypointSymbol, "market")}
	var market model.Market
	if err := c.Send("GET", &uriRef, nil, &market); err != nil {
		return nil, fmt.Errorf("failed API request: %w", err)
	}
	if err := db.SaveMarket(&market); err != nil {
		return nil, fmt.Errorf("failed to save market %s: %w", market.Symbol, err)
	}
	return &market, nil
}

func (c *Client) GetShipyard(waypointSymbol string, db *database.DB) (*model.Shipyard, error) {
	if shipyard, err := db.LoadShipyard(waypointSymbol); err == nil && shipyard != nil &&
		(shipyard.Ships != nil) { // TODO || !IsThereAShipAtWaypoint(waypoint)) {
		// TODO check last updated time
		return shipyard, nil
	}
	systemSymbol := WaypointSymbolToSystemSymbol(waypointSymbol)
	uriRef := url.URL{Path: path.Join("systems", systemSymbol, "waypoints", waypointSymbol, "shipyard")}
	var shipyard model.Shipyard
	if err := c.Send("GET", &uriRef, nil, &shipyard); err != nil {
		return nil, fmt.Errorf("failed API request: %w", err)
	}
	if err := db.SaveShipyard(&shipyard); err != nil {
		return nil, fmt.Errorf("failed to save shipyard %s: %w", shipyard.Symbol, err)
	}
	return &shipyard, nil
}

func (c *Client) GetSystem(systemSymbol string, db *database.DB) (*model.System, error) {
	if system, err := db.LoadSystem(systemSymbol); err == nil && system != nil {
		return system, nil
	}
	uriRef := url.URL{Path: path.Join("systems", systemSymbol)}
	var system model.System
	if err := c.Send("GET", &uriRef, nil, &system); err != nil {
		return nil, fmt.Errorf("failed API request: %w", err)
	}
	if err := db.SaveSystem(&system); err != nil {
		return nil, fmt.Errorf("failed to save system %s: %w", system.Symbol, err)
	}
	return &system, nil
}

func (c *Client) GetWaypoint(waypointSymbol string, db *database.DB) (*model.Waypoint, error) {
	if waypoint, err := db.LoadWaypoint(waypointSymbol); err == nil && waypoint != nil {
		// TODO check last updated time
		return waypoint, nil
	}
	systemSymbol := WaypointSymbolToSystemSymbol(waypointSymbol)
	uriRef := url.URL{Path: path.Join("systems", systemSymbol, "waypoints", waypointSymbol)}
	var waypoint model.Waypoint
	if err := c.Send("GET", &uriRef, nil, &waypoint); err != nil {
		return nil, fmt.Errorf("failed API request: %w", err)
	}
	if err := db.SaveWaypoint(&waypoint); err != nil {
		return nil, fmt.Errorf("failed to save waypoint %s: %w", waypoint.Symbol, err)
	}
	return &waypoint, nil
}

func (c *Client) ListWaypointsInSystem(systemSymbol string, db *database.DB) ([]model.Waypoint, error) {
	if waypoints, err := db.LoadWaypointsInSystem(systemSymbol); err == nil && waypoints != nil {
		// TODO check last updated time
		return waypoints, nil
	}
	uriRef := url.URL{Path: path.Join("systems", systemSymbol, "waypoints")}
	var waypoints []model.Waypoint
	if err := c.Send("GET", &uriRef, nil, &waypoints); err != nil {
		return nil, fmt.Errorf("failed API request: %w", err)
	}
	for _, waypoint := range waypoints {
		if err := db.SaveWaypoint(&waypoint); err != nil {
			return nil, fmt.Errorf("failed to save waypoint %s: %w", waypoint.Symbol, err)
		}
	}
	return waypoints, nil
}
