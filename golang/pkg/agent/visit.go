package agent

import (
	"fmt"
	"slices"

	"git.adyxax.org/adyxax/spacetraders/golang/pkg/model"
)

func (a *agent) visitAllShipyards(ship *model.Ship) error {
	shipyards, err := a.listShipyardsInSystem(ship.Nav.SystemSymbol)
	if err != nil {
		return fmt.Errorf("failed to list shipyards in system %s: %w", ship.Nav.SystemSymbol, err)
	}
	shipyards = slices.DeleteFunc(shipyards, func(shipyard model.Shipyard) bool {
		// filter out shipyards for which we already have ships prices
		if shipyard.Ships != nil {
			return true
		}
		// filter out shipyards for which a ship is either present or inbound
		return a.isThereAShipAtWaypoint(shipyard.Symbol)
	})
	if len(shipyards) == 0 {
		return nil
	}
	waypoint, err := a.client.GetWaypoint(ship.Nav.WaypointSymbol, a.db)
	if err != nil {
		return fmt.Errorf("failed to get nav waypoint %s: %w", ship.Nav.WaypointSymbol, err)
	}
	waypoints := make([]model.Waypoint, 0)
	for i := range shipyards {
		waypoint, err := a.client.GetWaypoint(shipyards[i].Symbol, a.db)
		if err != nil {
			return fmt.Errorf("failed to get waypoint %s: %w", shipyards[i].Symbol, err)
		}
		waypoints = append(waypoints, *waypoint)
	}
	sortByDistanceFrom(waypoint, waypoints)
	if err := a.client.Navigate(ship, waypoints[0].Symbol, a.db); err != nil {
		return fmt.Errorf("failed to navigate to %s: %w", waypoints[0].Symbol, err)
	}
	if _, err := a.client.GetShipyard(&waypoints[0], a.db); err != nil {
		return fmt.Errorf("failed to get shipyard %s: %w", waypoints[0].Symbol, err)
	}
	// TODO get market data
	return a.visitAllShipyards(ship)
}
