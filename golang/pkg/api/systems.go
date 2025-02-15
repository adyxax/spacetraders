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

func (c *Client) ListWaypointsInSystem(system *model.System, db *database.DB) ([]model.Waypoint, error) {
	// TODO database caching
	// TODO pagination
	// TODO check updated
	uriRef := url.URL{Path: path.Join("systems", system.Symbol, "waypoints")}
	var waypoints []model.Waypoint
	if err := c.Send("GET", &uriRef, nil, &waypoints); err != nil {
		return nil, fmt.Errorf("failed to list waypoints in system %s: %w", system.Symbol, err)
	}
	for _, waypoint := range waypoints {
		if err := db.SaveWaypoint(&waypoint); err != nil {
			return nil, fmt.Errorf("failed to list waypoints in system %s: %w", system.Symbol, err)
		}
	}
	return waypoints, nil
}

func (c *Client) GetWaypoint(symbol string, db *database.DB) (*model.Waypoint, error) {
	// TODO check updated
	if waypoint, err := db.LoadWaypoint(symbol); err == nil && waypoint != nil {
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
