package agent

import (
	"fmt"
	"slices"

	"git.adyxax.org/adyxax/spacetraders/golang/pkg/model"
)

type TradeGoodNotFoundError struct{}

func (err *TradeGoodNotFoundError) Error() string {
	return "trade good not found"
}

func (a *agent) buyTradeGood(ship *model.Ship, tradeGoodToBuy string) error {
	if ship.Cargo.Units == ship.Cargo.Capacity {
		return nil
	}
	// list markets would sell our goods
	markets, err := a.listMarketsInSystem(ship.Nav.SystemSymbol)
	if err != nil {
		return fmt.Errorf("failed to list markets in system %s: %w", ship.Nav.SystemSymbol, err)
	}
	markets = slices.DeleteFunc(markets, func(market model.Market) bool {
		for _, item := range market.Exports {
			if item.Symbol == tradeGoodToBuy {
				return false
			}
		}
		return true
	})
	if len(markets) == 0 {
		return &TradeGoodNotFoundError{}
	}
	// find the closest place to buy TODO
	waypoint, err := a.client.GetWaypoint(ship.Nav.WaypointSymbol)
	if err != nil {
		return fmt.Errorf("failed to get waypoint %s: %w", ship.Nav.WaypointSymbol, err)
	}
	waypoints := make([]model.Waypoint, 0)
	for i := range markets {
		waypoint, err := a.client.GetWaypoint(markets[i].Symbol)
		if err != nil {
			return fmt.Errorf("failed to get waypoint %s: %w", markets[i].Symbol, err)
		}
		waypoints = append(waypoints, *waypoint)
	}
	sortByDistanceFrom(waypoint, waypoints)
	// Go there and refresh our market data
	if err := a.navigate(ship, waypoints[0].Symbol); err != nil {
		return fmt.Errorf("failed to navigate to %s: %w", waypoints[0].Symbol, err)
	}
	if err := a.client.Dock(ship); err != nil {
		return fmt.Errorf("failed to dock: %w", err)
	}
	market, err := a.client.GetMarket(waypoints[0].Symbol, a.ships)
	if err != nil {
		return fmt.Errorf("failed to get market %s: %w", waypoints[0].Symbol, err)
	}
	// Buy until full
	for _, tradeGood := range market.TradeGoods {
		if tradeGood.Type == "EXPORT" && tradeGood.Symbol == tradeGoodToBuy {
			for ship.Cargo.Units < ship.Cargo.Capacity {
				increment := min(ship.Cargo.Capacity-ship.Cargo.Units, tradeGood.TradeVolume)
				if err := a.client.Purchase(ship, tradeGoodToBuy, increment); err != nil {
					return fmt.Errorf("failed to purchase %d units of %s: %w", increment, tradeGoodToBuy, err)
				}
			}
			break
		}
	}
	return a.buyTradeGood(ship, tradeGoodToBuy)
}

func (a *agent) sellEverythingExcept(ship *model.Ship, keep string) error {
	// First lets see what we need to sell
	var inventoryItems []model.InventoryItem
	for _, item := range ship.Cargo.Inventory {
		if item.Symbol != keep {
			inventoryItems = append(inventoryItems, item)
		}
	}
	if len(inventoryItems) == 0 {
		return nil
	}
	// list markets would buy our goods
	markets, err := a.listMarketsInSystem(ship.Nav.SystemSymbol)
	if err != nil {
		return fmt.Errorf("failed to list markets in system %s: %w", ship.Nav.SystemSymbol, err)
	}
	markets = slices.DeleteFunc(markets, func(market model.Market) bool {
		for _, marketItem := range market.Imports {
			for _, inventoryItem := range inventoryItems {
				if marketItem.Symbol == inventoryItem.Symbol {
					return false
				}
			}
		}
		return true
	})
	if len(markets) == 0 {
		return nil
	}
	// find the closest place to sell something TODO
	waypoint, err := a.client.GetWaypoint(ship.Nav.WaypointSymbol)
	if err != nil {
		return fmt.Errorf("failed to get nav waypoint %s: %w", ship.Nav.WaypointSymbol, err)
	}
	waypoints := make([]model.Waypoint, 0)
	for i := range markets {
		waypoint, err := a.client.GetWaypoint(markets[i].Symbol)
		if err != nil {
			return fmt.Errorf("failed to get waypoint %s: %w", markets[i].Symbol, err)
		}
		waypoints = append(waypoints, *waypoint)
	}
	sortByDistanceFrom(waypoint, waypoints)
	// Go there and refresh our market data
	if err := a.navigate(ship, waypoints[0].Symbol); err != nil {
		return fmt.Errorf("failed to navigate to %s: %w", waypoints[0].Symbol, err)
	}
	if err := a.client.Dock(ship); err != nil {
		return fmt.Errorf("failed to dock: %w", err)
	}
	market, err := a.client.GetMarket(waypoints[0].Symbol, a.ships)
	if err != nil {
		return fmt.Errorf("failed to get market %s: %w", waypoints[0].Symbol, err)
	}
	// sell everything we can
	for _, cargoItem := range inventoryItems {
		units := cargoItem.Units
		for _, tradeGood := range market.TradeGoods {
			if tradeGood.Type == "IMPORT" && tradeGood.Symbol == cargoItem.Symbol {
				for units > 0 {
					increment := min(units, tradeGood.TradeVolume)
					if err := a.client.Sell(ship, cargoItem.Symbol, increment); err != nil {
						return fmt.Errorf("failed to sell %d units of %s: %w", units, cargoItem.Symbol, err)
					}
					units = units - increment
				}
				break
			}
		}
	}
	return a.sellEverythingExcept(ship, keep)
}
