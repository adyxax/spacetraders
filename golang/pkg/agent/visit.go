package agent

import (
	"fmt"
	"log/slog"

	"git.adyxax.org/adyxax/spacetraders/golang/pkg/model"
)

func (a *agent) visitAllShipyards(ship *model.Ship) error {
	system, err := a.client.GetSystem(ship.Nav.SystemSymbol, a.db)
	if err != nil {
		return fmt.Errorf("failed to visit all shipyards: %w", err)
	}
	waypoints, err := a.client.ListWaypointsInSystem(system, a.db)
	if err != nil {
		return fmt.Errorf("failed to visit all shipyards: %w", err)
	}
	//slog.Info("get system", "system", system.Waypoints, "err", err)
	//waypoint, err := a.client.GetWaypoint("X1-RR14-J88", a.db)
	slog.Info("get waypoint", "waypoint", waypoints[0])

	return fmt.Errorf("failed to visit all shipyards: not implemented yet")
}
