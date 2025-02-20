package api

import (
	"strings"

	"git.adyxax.org/adyxax/spacetraders/golang/pkg/model"
)

func isThereAShipDockerAtWaypoint(symbol string, ships []model.Ship) bool {
	for _, ship := range ships {
		if ship.Nav.WaypointSymbol == symbol && ship.Nav.Status == "DOCKED" {
			return true
		}
	}
	return false
}

func WaypointSymbolToSystemSymbol(symbol string) string {
	return strings.Join(strings.Split(symbol, "-")[:2], "-")
}
