package api

import (
	"fmt"
	"net/url"
	"path"

	"git.adyxax.org/adyxax/spacetraders/golang/pkg/database"
	"git.adyxax.org/adyxax/spacetraders/golang/pkg/model"
)

func (c *Client) GetSystem(symbol string, db *database.DB) (*model.System, error) {
	if system, err := db.LoadSystem(symbol); err == nil && system != nil {
		return system, nil
	}
	uriRef := url.URL{Path: path.Join("systems", symbol)}
	var system model.System
	if err := c.Send("GET", &uriRef, nil, &system); err != nil {
		return nil, fmt.Errorf("failed to get system %s: %w", symbol, err)
	}
	if err := db.SaveSystem(&system); err != nil {
		return nil, fmt.Errorf("failed to get system %s: %w", symbol, err)
	}
	return &system, nil
}

func (c *Client) GetShipyard(waypoint *model.Waypoint, db *database.DB) (*model.Shipyard, error) {
	if shipyard, err := db.LoadShipyard(waypoint.Symbol); err == nil && shipyard != nil &&
		(shipyard.Ships != nil) { // TODO || !IsThereAShipAtWaypoint(waypoint)) {
		// TODO check last updated time
		return shipyard, nil
	}
	uriRef := url.URL{Path: path.Join("systems", waypoint.SystemSymbol, "waypoints", waypoint.Symbol, "shipyard")}
	var shipyard model.Shipyard
	if err := c.Send("GET", &uriRef, nil, &shipyard); err != nil {
		return nil, fmt.Errorf("failed to get shipyard at %s: %w", waypoint.Symbol, err)
	}
	if err := db.SaveShipyard(&shipyard); err != nil {
		return nil, fmt.Errorf("failed to get shipyard at %s: %w", waypoint.Symbol, err)
	}
	return &shipyard, nil
}

func (c *Client) GetWaypoint(symbol string, db *database.DB) (*model.Waypoint, error) {
	if waypoint, err := db.LoadWaypoint(symbol); err == nil && waypoint != nil {
		// TODO check last updated time
		return waypoint, nil
	}
	systemSymbol := WaypointSymbolToSystemSymbol(symbol)
	uriRef := url.URL{Path: path.Join("systems", systemSymbol, "waypoints", symbol)}
	var waypoint model.Waypoint
	if err := c.Send("GET", &uriRef, nil, &waypoint); err != nil {
		return nil, fmt.Errorf("failed to get waypoint %s: %w", symbol, err)
	}
	if err := db.SaveWaypoint(&waypoint); err != nil {
		return nil, fmt.Errorf("failed to get waypoint %s: %w", symbol, err)
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
		return nil, fmt.Errorf("failed to list waypoints in system %s: %w", systemSymbol, err)
	}
	for _, waypoint := range waypoints {
		if err := db.SaveWaypoint(&waypoint); err != nil {
			return nil, fmt.Errorf("failed to list waypoints in system %s: %w", systemSymbol, err)
		}
	}
	return waypoints, nil
}
