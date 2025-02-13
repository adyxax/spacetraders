package agent

import (
	"errors"
	"fmt"
	"log/slog"

	"git.adyxax.org/adyxax/spacetraders/golang/pkg/api"
	"git.adyxax.org/adyxax/spacetraders/golang/pkg/database"
)

func Run(
	apiClient *api.Client,
	db *database.DB,
	getenv func(string) string,
) error {
	accountToken := getenv("SPACETRADERS_ACCOUNT_TOKEN")
	if accountToken == "" {
		return fmt.Errorf("the SPACETRADERS_ACCOUNT_TOKEN environment variable is not set")
	}
	agent := getenv("SPACETRADERS_AGENT")
	if agent == "" {
		return fmt.Errorf("the SPACETRADERS_AGENT environment variable is not set")
	}
	faction := getenv("SPACETRADERS_FACTION")
	if faction == "" {
		return fmt.Errorf("the SPACETRADERS_FACTION environment variable is not set")
	}
	// ----- Get token or register ---------------------------------------------
	apiClient.SetToken(accountToken)
	register, err := apiClient.Register(faction, agent)
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
	contracts, err := apiClient.MyContracts()
	if err != nil {
		return err
	}
	slog.Info("start", "contract", contracts[0], "err", err)
	ships, err := apiClient.MyShips()
	if err != nil {
		return err
	}
	slog.Info("start", "ship", ships[0].Nav.Status, "err", err)
	return nil
}
