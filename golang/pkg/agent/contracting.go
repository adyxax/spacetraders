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
				a.channel <- fmt.Errorf("failed to run contracts with ship %s: %w", ship.Symbol, err)
				return
			}
		}
	}
	a.channel <- fmt.Errorf("failed to run contracts: negotiating new contracts is not implemented yet")
	// TODO
	//for {
	// negotiate
	// runContract
	//}
}

func (a *agent) runContract(contract *model.Contract, ship *model.Ship) error {
	if err := a.client.Accept(contract, a.db); err != nil {
		return fmt.Errorf("failed to run contract: %w", err)
	}
	//slog.Info("running contract", "contract", contract, "ship", ship.Symbol)
	switch contract.Type {
	// TODO
	//case "PROCUREMENT":
	default:
		return fmt.Errorf("failed to run contract: handling contracts of type %s is not implemented yet", contract.Type)
	}
	return nil
}
