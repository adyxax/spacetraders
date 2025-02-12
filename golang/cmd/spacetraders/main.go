package main

import (
	"context"
	"errors"
	"fmt"
	"log/slog"
	"os"
	"os/signal"

	"git.adyxax.org/adyxax/spacetraders/golang/pkg/api"
	"git.adyxax.org/adyxax/spacetraders/golang/pkg/database"
	"git.adyxax.org/adyxax/spacetraders/golang/pkg/lib"
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

	db, err := database.NewDB(
		ctx,
		"./spacetraders.db?_txlock=immediate",
	)
	if err != nil {
		fmt.Fprintf(os.Stderr, "failed to open database: %+v\n", err)
		os.Exit(1)
	}

	apiClient := api.NewClient(ctx)
	defer apiClient.Close()
	if err := run(
		apiClient,
		db,
	); err != nil {
		fmt.Fprintf(os.Stderr, "%s\n", err)
		if err := db.Close(); err != nil {
			fmt.Fprintf(os.Stderr, "%s\n", err)
		}
		os.Exit(2)
	}
}

func run(
	apiClient *api.Client,
	db *database.DB,
) error {
	// ----- Get token or register ---------------------------------------------
	register, err := apiClient.Register("COSMIC", "ADYXAX-GO")
	if err != nil {
		apiError := &api.APIError{}
		if errors.As(err, &apiError) {
			switch apiError.Code {
			case 4111: // Agent symbol has already been claimed
				token, err := db.GetToken()
				if err != nil || token == "" {
					return fmt.Errorf("failed to register and failed to get a token from the database: someone stole our agent's callsign: %w", err)
				}
				apiClient.SetToken(token)
				agent, err := apiClient.MyAgent()
				if err != nil {
					return fmt.Errorf("failed to get agent: %w", err)
				}
				slog.Info("agent", "/my/agent", agent)
			default:
				return fmt.Errorf("failed to register: %w", err)
			}
		} else {
			return fmt.Errorf("failed to register with an invalid apiError: %w", err)
		}
	} else {
		token, err := db.GetToken()
		if err != nil || token == "" {
			if err := db.AddToken(register.Token); err != nil {
				return fmt.Errorf("failed to save token: %w", err)
			}
			apiClient.SetToken(register.Token)
		} else {
			// We successfully registered but have a tainted database
			slog.Error("token", "token", register.Token)
			return fmt.Errorf("TODO server reset not implemented yet")
		}
	}
	// ----- run agent ---------------------------------------------------------
	ships, err := apiClient.MyShips()
	if err != nil {
		return err
	}
	slog.Info("start", "ship", ships[0].Nav.Status, "err", err)
	err = apiClient.Orbit(&ships[0])
	if err != nil {
		return err
	}
	slog.Info("orbit", "ship", ships[0].Nav.Status, "err", err)
	err = apiClient.Dock(&ships[0])
	if err != nil {
		return err
	}
	slog.Info("dock", "ship", ships[0].Nav.Status, "err", err)
	return nil
}
