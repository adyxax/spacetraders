package agent

import (
	"fmt"
	"time"

	"git.adyxax.org/adyxax/spacetraders/golang/pkg/model"
)

func (a *agent) autoContracting(ship *model.Ship) {
	defer a.wg.Done()
	contracts, err := a.client.MyContracts()
	if err != nil {
		a.channel <- fmt.Errorf("failed to get my contracts with ship %s: %w", ship.Symbol, err)
		return
	}
	for _, contract := range contracts {
		if contract.Fullfilled {
			continue
		}
		now := time.Now()
		if now.Before(contract.Terms.Deadline) {
			if err := a.runContract(&contract, ship); err != nil {
				a.channel <- fmt.Errorf("failed to run contract %s with ship %s: %w", contract.Id, ship.Symbol, err)
				return
			}
		}
	}
	a.channel <- fmt.Errorf("negotiating new contracts is not implemented yet")
	// TODO
	//for {
	// negotiate
	// runContract
	//}
}

func (a *agent) runContract(contract *model.Contract, ship *model.Ship) error {
	if err := a.client.Accept(contract, a.db); err != nil {
		return fmt.Errorf("failed to accept contract: %w", err)
	}
	//slog.Info("running contract", "contract", contract, "ship", ship.Symbol)
	switch contract.Type {
	case "PROCUREMENT":
		if err := a.runProcurement(contract, ship); err != nil {
			return fmt.Errorf("failed to run procurement: %w", err)
		}
	default:
		return fmt.Errorf("handling contracts of type %s is not implemented yet", contract.Type)
	}
	return nil
}

func (a *agent) runProcurement(contract *model.Contract, ship *model.Ship) error {
	deliveryCargo := contract.Terms.Deliver[0].TradeSymbol
	deliveryWaypoint, err := a.client.GetWaypoint(contract.Terms.Deliver[0].DestinationSymbol, a.db)
	if err != nil {
		return fmt.Errorf("failed to get delivery waypoint: %w", err)
	}
	for !contract.Fullfilled {
		_ = deliveryCargo
		_ = deliveryWaypoint
		return fmt.Errorf("not implemented")
	}
	return fmt.Errorf("not implemented")
}
