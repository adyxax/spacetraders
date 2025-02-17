package database

import (
	"encoding/json"
	"fmt"

	"git.adyxax.org/adyxax/spacetraders/golang/pkg/model"
)

func (db *DB) AppendTransaction(transaction *model.Transaction) error {
	data, err := json.Marshal(transaction)
	if err != nil {
		return fmt.Errorf("failed to marshal transaction: %w", err)
	}
	if _, err := db.Exec(`INSERT INTO transactions(data) VALUES (json(?));`, data); err != nil {
		return fmt.Errorf("failed to exec: %w", err)
	}
	return nil
}
