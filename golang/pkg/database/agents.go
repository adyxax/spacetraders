package database

import (
	"encoding/json"
	"fmt"

	"git.adyxax.org/adyxax/spacetraders/golang/pkg/model"
)

func (db *DB) SaveAgent(agent *model.Agent) error {
	data, err := json.Marshal(agent)
	if err != nil {
		return fmt.Errorf("failed to marshal agent: %w", err)
	}
	if _, err := db.Exec(`INSERT INTO agents VALUES data = (json(?));`, data); err != nil {
		return fmt.Errorf("failed to insert agent data: %w", err)
	}
	return nil
}
