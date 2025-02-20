package agent

import (
	"container/heap"
	"fmt"
	"log/slog"
	"math"
	"slices"

	"git.adyxax.org/adyxax/spacetraders/golang/pkg/api"
	"git.adyxax.org/adyxax/spacetraders/golang/pkg/model"
)

func (a *agent) navigate(ship *model.Ship, waypointSymbol string) error {
	if ship.Nav.WaypointSymbol == waypointSymbol {
		return nil
	}
	if ship.Fuel.Capacity == 0 {
		if err := a.client.Navigate(ship, waypointSymbol); err != nil {
			return fmt.Errorf("failed to navigate to %s: %w", waypointSymbol, err)
		}
	}
	path, cost, err := a.shortestPath(ship.Nav.WaypointSymbol, waypointSymbol, ship.Fuel.Capacity)
	if err != nil {
		return fmt.Errorf("failed to compute shortest path: %w", err)
	}
	slog.Debug("shortest path", "origin", ship.Nav.WaypointSymbol, "destination", waypointSymbol, "path", path, "cost", cost)
	for _, next := range path {
		if err := a.client.Refuel(ship); err != nil {
			return fmt.Errorf("failed to refuel: %w", err)
		}
		if err := a.client.Navigate(ship, next); err != nil {
			return fmt.Errorf("failed to navigate to %s: %w", next, err)
		}
	}
	return nil
}

func (a *agent) shortestPath(origin string, destination string, fuelCapacity int) ([]string, int, error) {
	if fuelCapacity == 0 { // Probes
		fuelCapacity = math.MaxInt
	}
	systemSymbol := api.WaypointSymbolToSystemSymbol(origin)
	waypoints, err := a.client.ListWaypointsInSystem(systemSymbol)
	if err != nil {
		return nil, 0, fmt.Errorf("failed to list waypoints in system %s: %w", systemSymbol, err)
	}
	backtrace := make(map[string]string) // backtrace map with the shortest path from one point to another
	costs := make(map[string]int)        // cost to reach each waypoint from the origin. cost = distance + 1
	costs[origin] = 0
	unvisited := make(map[string]*model.Waypoint)
	for i := range waypoints {
		symbol := waypoints[i].Symbol
		costs[symbol] = math.MaxInt
		unvisited[waypoints[i].Symbol] = &waypoints[i]
		// We need to know which waypoints allow refueling
		waypoints[i].Traits = slices.DeleteFunc(waypoints[i].Traits, func(trait model.Common) bool {
			if trait.Symbol == "MARKETPLACE" {
				return false
			}
			return true
		})
		if len(waypoints[i].Traits) > 0 {
			market, err := a.client.GetMarket(symbol, a.ships)
			if err != nil {
				return nil, 0, fmt.Errorf("failed to get market %s: %w", symbol, err)
			}
			market.Exchange = slices.DeleteFunc(market.Exchange, func(item model.Common) bool {
				if item.Symbol == "FUEL" {
					return false
				}
				return true
			})
			market.Exports = slices.DeleteFunc(market.Exports, func(item model.Common) bool {
				if item.Symbol == "FUEL" {
					return false
				}
				return true
			})
			if len(market.Exchange) == 0 && len(market.Exports) == 0 {
				waypoints[i].Traits = nil
			}
		}
	}
	costs[origin] = 0
	pq := make(PriorityQueue, 1)
	pq[0] = &Node{
		waypointSymbol: origin,
	}
	heap.Init(&pq)
outer:
	for pq.Len() > 0 {
		node := heap.Pop(&pq).(*Node)
		waypoint, ok := unvisited[node.waypointSymbol]
		if !ok { // already visited
			continue
		}
		delete(unvisited, node.waypointSymbol)
		for _, candidate := range unvisited {
			fuel := int(math.Floor(math.Sqrt(float64(distance2(waypoint, candidate))))) + 1
			if fuel > fuelCapacity {
				continue
			}
			cost := node.cost + fuel
			if cost < costs[candidate.Symbol] {
				backtrace[candidate.Symbol] = node.waypointSymbol
				costs[candidate.Symbol] = cost
				if candidate.Symbol == destination {
					break outer
				}
				if len(candidate.Traits) > 0 {
					heap.Push(&pq, &Node{
						cost:           cost,
						waypointSymbol: candidate.Symbol,
					})
				}
			}
		}
	}
	path := []string{destination}
	step, ok := backtrace[destination]
	if !ok {
		slog.Debug("shortest path failure", "backtraces", backtrace, "costs", costs)
		return nil, 0, fmt.Errorf("no path exists between origin and destination with the given fuel capacity")
	}
	for step != origin {
		path = append([]string{step}, path...)
		step = backtrace[step]
	}
	return path, costs[destination], nil
}

// Priority queue implementation with container/heap
type Node struct {
	cost           int
	waypointSymbol string
	index          int // needed by the heap.Interface methods
}

type PriorityQueue []*Node

func (pq PriorityQueue) Len() int           { return len(pq) }
func (pq PriorityQueue) Less(i, j int) bool { return pq[i].cost < pq[j].cost }
func (pq PriorityQueue) Swap(i, j int) {
	pq[i], pq[j] = pq[j], pq[i]
	pq[i].index = i
	pq[j].index = j
}
func (pq *PriorityQueue) Push(x any) {
	n := len(*pq)
	item := x.(*Node)
	item.index = n
	*pq = append(*pq, item)
}
func (pq *PriorityQueue) Pop() any {
	old := *pq
	n := len(old)
	item := old[n-1]
	old[n-1] = nil  // don't stop the GC from reclaiming the item eventually
	item.index = -1 // for safety
	*pq = old[0 : n-1]
	return item
}
func (pq *PriorityQueue) Update(n *Node, cost int) {
	n.cost = cost
	heap.Fix(pq, n.index)
}
