package agent

import (
	"cmp"
	"fmt"
	"math"
	"slices"

	"git.adyxax.org/adyxax/spacetraders/golang/pkg/model"
)

func distance2(a *model.Waypoint, b *model.Waypoint) int {
	x2 := a.X - b.X
	y2 := a.Y - b.Y
	return x2*x2 + y2*y2
}

func (a *agent) isThereAShipAtWaypoint(waypointSymbol string) bool {
	for _, ship := range a.ships {
		if ship.Nav.WaypointSymbol == waypointSymbol {
			return true
		}
	}
	return false
}

func (a *agent) listWaypointsInSystemWithTrait(systemSymbol string, trait string) ([]model.Waypoint, error) {
	waypoints, err := a.client.ListWaypointsInSystem(systemSymbol, a.db)
	if err != nil {
		return nil, fmt.Errorf("failed to list waypoints: %w", err)
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

func (a *agent) listMarketsInSystem(systemSymbol string) ([]model.Market, error) {
	waypoints, err := a.listWaypointsInSystemWithTrait(systemSymbol, "MARKETPLACE")
	if err != nil {
		return nil, fmt.Errorf("failed to list waypoints in system %s with trait MARKETPLACE: %w", systemSymbol, err)
	}
	var markets []model.Market
	for i := range waypoints {
		market, err := a.client.GetMarket(waypoints[i].Symbol, a.db)
		if err != nil {
			return nil, fmt.Errorf("failed to get market %s: %w", waypoints[i].Symbol, err)
		}
		markets = append(markets, *market)
	}
	return markets, nil
}

func (a *agent) listShipyardsInSystem(systemSymbol string) ([]model.Shipyard, error) {
	waypoints, err := a.listWaypointsInSystemWithTrait(systemSymbol, "SHIPYARD")
	if err != nil {
		return nil, fmt.Errorf("failed to list waypoints in system %s with trait SHIPYARD: %w", systemSymbol, err)
	}
	var shipyards []model.Shipyard
	for i := range waypoints {
		shipyard, err := a.client.GetShipyard(waypoints[i].Symbol, a.db)
		if err != nil {
			return nil, fmt.Errorf("failed to get shipyard %s: %w", waypoints[i].Symbol, err)
		}
		shipyards = append(shipyards, *shipyard)
	}
	return shipyards, nil
}

func (a *agent) sendShipToShipyardThatSells(ship *model.Ship, shipType string) error {
	shipyards, err := a.listShipyardsInSystem(ship.Nav.SystemSymbol)
	if err != nil {
		return fmt.Errorf("failed to list shipyards in system %s: %w", ship.Nav.SystemSymbol, err)
	}
	// filter out the shipyards that do not sell our ship
	shipyards = slices.DeleteFunc(shipyards, func(shipyard model.Shipyard) bool {
		for _, t := range shipyard.ShipTypes {
			if t.Type == shipType {
				return false
			}
		}
		return true
	})
	// sort by cheapest
	slices.SortFunc(shipyards, func(a, b model.Shipyard) int {
		aPrice := math.MaxInt
		for _, ship := range a.Ships {
			if ship.Type == shipType {
				aPrice = ship.PurchasePrice
				break
			}
		}
		bPrice := math.MaxInt
		for _, ship := range b.Ships {
			if ship.Type == shipType {
				bPrice = ship.PurchasePrice
				break
			}
		}
		return cmp.Compare(aPrice, bPrice)
	})
	if err := a.client.Navigate(ship, shipyards[0].Symbol, a.db); err != nil {
		return fmt.Errorf("failed to navigate to %s: %w", shipyards[0].Symbol, err)
	}
	return nil
}

func sortByDistanceFrom(origin *model.Waypoint, destinations []model.Waypoint) {
	slices.SortFunc(destinations, func(a, b model.Waypoint) int {
		return cmp.Compare(distance2(origin, &a), distance2(origin, &b))
	})
}
