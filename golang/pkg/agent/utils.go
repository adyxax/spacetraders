package agent

import (
	"cmp"
	"fmt"
	"slices"

	"git.adyxax.org/adyxax/spacetraders/golang/pkg/model"
)

type Point interface {
	GetX() int
	GetY() int
}

func distance2(a Point, b Point) int {
	x2 := a.GetX() - b.GetX()
	y2 := a.GetY() - b.GetY()
	return x2*x2 + y2*y2
}

func (a *agent) isThereAShipAtWaypoint(waypoint *model.Waypoint) bool {
	for _, ship := range a.ships {
		if ship.Nav.WaypointSymbol == waypoint.Symbol {
			return true
		}
	}
	return false
}

func (a *agent) listWaypointsInSystemWithTrait(system *model.System, trait string) ([]model.Waypoint, error) {
	waypoints, err := a.client.ListWaypointsInSystem(system, a.db)
	if err != nil {
		return nil, fmt.Errorf("failed to list waypoints with trait: %w", err)
	}
	waypoints = slices.DeleteFunc(waypoints, func(waypoint model.Waypoint) bool {
		for _, t := range waypoint.Traits {
			if t.Symbol == trait {
				return false
			}
		}
		return true
	})
	return waypoints, nil
}

func (a *agent) listShipyardsInSystem(system *model.System) ([]model.Shipyard, error) {
	waypoints, err := a.listWaypointsInSystemWithTrait(system, "SHIPYARD")
	if err != nil {
		return nil, fmt.Errorf("failed to list shipyards in system: %w", err)
	}
	var shipyards []model.Shipyard
	for i := range waypoints {
		shipyard, err := a.client.GetShipyard(&waypoints[i], a.db)
		if err != nil {
			return nil, fmt.Errorf("failed to list shipyards in system: %w", err)
		}
		shipyards = append(shipyards, *shipyard)
	}
	return shipyards, nil
}

func sortByDistanceFrom[P Point](origin P, destinations []P) {
	slices.SortFunc(destinations, func(a, b P) int {
		return cmp.Compare(distance2(origin, a), distance2(origin, b))
	})
}
