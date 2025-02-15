package agent

import (
	"fmt"
	"log/slog"
	"sync"

	"git.adyxax.org/adyxax/spacetraders/golang/pkg/api"
	"git.adyxax.org/adyxax/spacetraders/golang/pkg/database"
	"git.adyxax.org/adyxax/spacetraders/golang/pkg/model"
)

type agent struct {
	channel chan shipError
	client  *api.Client
	db      *database.DB
	getenv  func(string) string
	ships   []model.Ship
	wg      sync.WaitGroup
}

type State int

const (
	start_running_contracts_with_the_command_ship = iota
	visit_all_shipyards
)

func Run(
	client *api.Client,
	db *database.DB,
	getenv func(string) string,
) error {
	agent := agent{
		channel: make(chan shipError),
		client:  client,
		db:      db,
		getenv:  getenv,
	}
	err := agent.init()
	if err != nil {
		return fmt.Errorf("failed to init agent: %w", err)
	}

	if agent.ships, err = client.MyShips(); err != nil {
		return fmt.Errorf("failed to init the agent's ships: %w", err)
	}
	var state State = start_running_contracts_with_the_command_ship
	agent.wg.Add(1)
	go func() {
		defer agent.wg.Done()
		for {
			switch state {
			case start_running_contracts_with_the_command_ship:
				agent.wg.Add(1)
				go agent.autoContracting(&agent.ships[0])
				state++
			case visit_all_shipyards:
				if err := agent.visitAllShipyards(&agent.ships[1]); err != nil {
					agent.sendShipError(fmt.Errorf("agent runner returned an error on state %d: %w", state, err), &agent.ships[1])
				}
				state++
				return
			default:
				agent.sendShipError(fmt.Errorf("agent runner reach an unknown state: %d", state), nil)
				return
			}
		}
	}()
	var errWg sync.WaitGroup
	errWg.Add(1)
	go func() {
		defer errWg.Done()
		for shipErr := range agent.channel {
			slog.Error("ship error", "err", shipErr.err, "ship", shipErr.ship.Symbol)
		}
	}()
	agent.wg.Wait()
	close(agent.channel)
	errWg.Wait()
	return nil
}
