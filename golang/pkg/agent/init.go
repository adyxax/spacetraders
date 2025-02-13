package agent

import (
	"errors"
	"fmt"
	"log/slog"

	"git.adyxax.org/adyxax/spacetraders/golang/pkg/api"
	"git.adyxax.org/adyxax/spacetraders/golang/pkg/model"
)

func (a *agent) init() error {
	token, err := a.db.GetToken()
	if err == nil && token != "" {
		a.client.SetToken(token)
		var agent *model.Agent
		agent, err = a.client.MyAgent() // we need err to carry over outside this block
		if err == nil {
			slog.Info("agent", "/my/agent", agent)
		} else {
			slog.Info("failed to get agent, handling server reset", "err", err)
			a.db.Reset()
		}
	}
	if err != nil || token == "" {
		// No token or invalid token (since we carry over the error of MyAgent() in the previous if), we need to register
		accountToken := a.getenv("SPACETRADERS_ACCOUNT_TOKEN")
		if accountToken == "" {
			return fmt.Errorf("the SPACETRADERS_ACCOUNT_TOKEN environment variable is not set")
		}
		a.client.SetToken(accountToken)
		agent := a.getenv("SPACETRADERS_AGENT")
		if agent == "" {
			return fmt.Errorf("the SPACETRADERS_AGENT environment variable is not set")
		}
		faction := a.getenv("SPACETRADERS_FACTION")
		if faction == "" {
			return fmt.Errorf("the SPACETRADERS_FACTION environment variable is not set")
		}
		register, err := a.client.Register(faction, agent)
		if err != nil {
			apiError := &api.APIError{}
			if errors.As(err, &apiError) {
				switch apiError.Code {
				case 4111: // Agent symbol has already been claimed
					return fmt.Errorf("failed to register and failed to get a token from the database: someone stole our agent's callsign: %w", err)
				default:
					return fmt.Errorf("failed to register: %w", err)
				}
			} else {
				return fmt.Errorf("failed to register with an invalid apiError: %w", err)
			}
		}
		a.client.SetToken(register.Token)
		if err := a.db.SaveToken(register.Token); err != nil {
			return fmt.Errorf("failed to save token %s after a successful registration: %w", register.Token, err)
		}
	}
	return nil
}
