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
	channel chan error
	client  *api.Client
	db      *database.DB
	getenv  func(string) string
	ships   []model.Ship
	wg      sync.WaitGroup
}

type State int

const (
	start_running_contracts_with_the_command_ship = iota
	visit_all_shipyards_with_the_starting_probe
	send_the_starting_probe_to_a_shipyard_that_sells_probes
)

func Run(
	client *api.Client,
	db *database.DB,
	getenv func(string) string,
) error {
	agent := agent{
		channel: make(chan error),
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
			case visit_all_shipyards_with_the_starting_probe:
				if err := agent.visitAllShipyards(&agent.ships[1]); err != nil {
					agent.channel <- fmt.Errorf("failed agent run: %w", err)
					return
				}
				state++
			case send_the_starting_probe_to_a_shipyard_that_sells_probes:
				if err := agent.sendShipToShipyardThatSells(&agent.ships[1], "SHIP_PROBE"); err != nil {
					agent.channel <- fmt.Errorf("failed agent run: %w", err)
					return
				}
				state++
			default:
				agent.channel <- fmt.Errorf("agent runner reach an unknown state: %d", state)
				return
			}
		}
	}()
	var errWg sync.WaitGroup
	errWg.Add(1)
	go func() {
		defer errWg.Done()
		for err := range agent.channel {
			slog.Error("error", "err", err)
		}
	}()
	agent.wg.Wait()
	close(agent.channel)
	errWg.Wait()
	return nil
}
