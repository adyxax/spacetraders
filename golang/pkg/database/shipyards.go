package database

import (
	"database/sql"
	"encoding/json"
	"fmt"
	"time"

	"git.adyxax.org/adyxax/spacetraders/golang/pkg/model"
)

func (db *DB) LoadShipyard(symbol string) (*model.Shipyard, error) {
	var buf []byte
	if err := db.QueryRow(`SELECT data FROM shipyards WHERE data->>'symbol' = ?;`, symbol).Scan(&buf); err != nil {
		return nil, fmt.Errorf("failed to query row: %w", err)
	}
	var shipyard model.Shipyard
	if err := json.Unmarshal(buf, &shipyard); err != nil {
		return nil, fmt.Errorf("failed to unmarshal shipyard: %w", err)
	}
	return &shipyard, nil
}

func (db *DB) SaveShipyard(shipyard *model.Shipyard) error {
	data, err := json.Marshal(shipyard)
	if err != nil {
		return fmt.Errorf("failed to marshal shipyard: %w", err)
	}
	_, err = db.Exec(
		`INSERT INTO shipyards(data, updated)
                VALUES (json(:data), :updated)
         ON CONFLICT DO UPDATE SET data = :data, updated = :updated
		                       WHERE data->>'symbol' = :symbol;`,
		sql.Named("data", data),
		sql.Named("symbol", shipyard.Symbol),
		sql.Named("updated", time.Now()),
	)
	if err != nil {
		return fmt.Errorf("failed to exec: %w", err)
	}
	return nil
}
