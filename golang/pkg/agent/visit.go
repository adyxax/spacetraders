package agent

import (
	"fmt"
	"slices"

	"git.adyxax.org/adyxax/spacetraders/golang/pkg/model"
)

func (a *agent) visitAllShipyards(ship *model.Ship) error {
	system, err := a.client.GetSystem(ship.Nav.SystemSymbol, a.db)
	if err != nil {
		return fmt.Errorf("failed to visit all shipyards: %w", err)
	}
	shipyards, err := a.listShipyardsInSystem(system)
	if err != nil {
		return fmt.Errorf("failed to visit all shipyards: %w", err)
	}
	shipyards = slices.DeleteFunc(shipyards, func(shipyard model.Shipyard) bool {
		// filter out shipyards for which we already have ships prices
		if shipyard.Ships != nil {
			return true
		}
		// filter out shipyards for which a ship is either present or inbound
		waypoint, err := a.client.GetWaypoint(shipyard.Symbol, a.db)
		if err != nil {
			panic(fmt.Errorf("failed to visit all shipyards: %w", err))
		}
		return a.isThereAShipAtWaypoint(waypoint)
	})
	if len(shipyards) == 0 {
		return nil
	}
	waypoint, err := a.client.GetWaypoint(ship.Nav.WaypointSymbol, a.db)
	if err != nil {
		return fmt.Errorf("failed to visit all shipyards: %w", err)
	}
	waypoints := make([]model.Waypoint, 0)
	for i := range shipyards {
		waypoint, err := a.client.GetWaypoint(shipyards[i].Symbol, a.db)
		if err != nil {
			return fmt.Errorf("failed to visit all shipyards: %w", err)
		}
		waypoints = append(waypoints, *waypoint)
	}
	sortByDistanceFrom(*waypoint, waypoints)
	if err := a.client.Navigate(ship, &waypoints[0], a.db); err != nil {
		return fmt.Errorf("failed to visit all shipyards: %w", err)
	}
	if _, err := a.client.GetShipyard(&waypoints[0], a.db); err != nil {
		return fmt.Errorf("failed to visit all shipyards: %w", err)
	}
	// TODO get market data
	return a.visitAllShipyards(ship)
}
