package api

import "strings"

func WaypointSymbolToSystemSymbol(symbol string) string {
	return strings.Join(strings.Split(symbol, "-")[:2], "-")
}
