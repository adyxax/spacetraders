package main

import (
	"context"
	"errors"
	"fmt"
	"log/slog"
	"os"
	"os/signal"

	"git.adyxax.org/adyxax/spacetraders/v2/pkg/api"
	"git.adyxax.org/adyxax/spacetraders/v2/pkg/database"
)

func main() {
	var opts *slog.HandlerOptions
	if os.Getenv("SPACETRADERS_DEBUG") != "" {
		opts = &slog.HandlerOptions{
			//AddSource: true,
			Level: slog.LevelDebug,
		}
	}
	logger := slog.New(slog.NewJSONHandler(os.Stdout, opts))
	slog.SetDefault(logger)

	ctx, cancel := signal.NotifyContext(context.Background(), os.Interrupt)
	defer cancel()

	db, err := database.DBInit(ctx, "./spacetraders.db")
	if err != nil {
		fmt.Fprintf(os.Stderr, "DbInit error %+v\n", err)
		os.Exit(1)
	}

	client := api.NewClient(ctx)
	defer client.Close()
	if err := run(
		db,
		client,
	); err != nil {
		fmt.Fprintf(os.Stderr, "%s\n", err)
		if err := db.Close(); err != nil {
			fmt.Fprintf(os.Stderr, "%s\n", err)
		}
		os.Exit(2)
	}
}

func run(
	db *database.DB,
	client *api.Client,
) error {
	// ----- Get token or register ---------------------------------------------
	register, err := client.Register("COSMIC", "ADYXAX-GO")
	if err != nil {
		apiError := &api.APIError{}
		if errors.As(err, &apiError) {
			switch apiError.Code {
			case 4111: // Agent symbol has already been claimed
				token, err := db.GetToken()
				if err != nil || token == "" {
					return fmt.Errorf("failed to register and failed to get a token from the database: someone stole our agent's callsign: %w", err)
				}
				client.SetToken(token)
				agent, err := client.MyAgent()
				if err != nil {
					return fmt.Errorf("failed to get agent: %w", err)
				}
				slog.Info("agent", "agent", agent)
			default:
				return fmt.Errorf("failed to register: %w\n", err)
			}
		} else {
			return fmt.Errorf("failed to register: %w\n", err)
		}
	} else {
		token, err := db.GetToken()
		if err != nil || token == "" {
			if err := db.AddToken(register.Token); err != nil {
				return fmt.Errorf("failed to save token: %w", err)
			}
			client.SetToken(register.Token)
		} else {
			return fmt.Errorf("TODO server reset not implemented yet")
		}
	}
	// ----- Get ships ---------------------------------------------------------
	ships, err := client.MyShips()
	err = client.Dock(&ships[0])
	slog.Info("dock", "ship", ships[0].Nav.Status, "err", err)
	err = client.Orbit(&ships[0])
	slog.Info("orbit", "ship", ships[0].Nav.Status, "err", err)
	return nil
}
