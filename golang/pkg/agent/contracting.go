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
		if contract.Fulfilled {
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
	for {
		contract, err := a.client.NegotiateContract(ship)
		if err != nil {
			a.channel <- fmt.Errorf("failed to negotiate contract: %w", err)
			return
		}
		if err := a.runContract(contract, ship); err != nil {
			a.channel <- fmt.Errorf("failed to run contract %s: %w", contract.Id, err)
			return
		}
	}
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
	if contract.Fulfilled {
		return nil
	}
	deliver := contract.Terms.Deliver[0]
	// make sure we are not carrying useless stuff
	if err := a.sellEverythingExcept(ship, deliver.TradeSymbol); err != nil {
		return fmt.Errorf("failed to sell everything except %s for ship %s: %w", deliver.TradeSymbol, ship.Symbol, err)
	}
	// procure the desired goods
	if ship.Cargo.Units < min(deliver.UnitsRequired-deliver.UnitsFulfilled, ship.Cargo.Capacity) {
		if err := a.buyTradeGood(ship, deliver.TradeSymbol); err != nil {
			return fmt.Errorf("failed to buy trade good %s with ship %s: %w", deliver.TradeSymbol, ship.Symbol, err)
		}
	}
	// deliver the goods
	if err := a.client.Navigate(ship, deliver.DestinationSymbol, a.db); err != nil {
		return fmt.Errorf("failed to navigate to %s: %w", deliver.DestinationSymbol, err)
	}
	if err := a.client.Deliver(contract, ship, a.db); err != nil {
		return fmt.Errorf("failed to deliver: %w", err)
	}
	deliver = contract.Terms.Deliver[0]
	if deliver.UnitsRequired == deliver.UnitsFulfilled {
		if err := a.client.Fulfill(contract, a.db); err != nil {
			return fmt.Errorf("failed to fulfill: %w", err)
		}
		return nil
	}
	return a.runProcurement(contract, ship)
}
