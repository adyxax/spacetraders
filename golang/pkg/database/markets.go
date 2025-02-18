package database

import (
	"database/sql"
	"encoding/json"
	"fmt"
	"time"

	"git.adyxax.org/adyxax/spacetraders/golang/pkg/model"
)

func (db *DB) LoadMarket(symbol string) (*model.Market, error) {
	var buf []byte
	if err := db.QueryRow(`SELECT data FROM markets WHERE data->>'symbol' = ?;`, symbol).Scan(&buf); err != nil {
		return nil, fmt.Errorf("failed to query row: %w", err)
	}
	var market model.Market
	if err := json.Unmarshal(buf, &market); err != nil {
		return nil, fmt.Errorf("failed to unmarshal market: %w", err)
	}
	return &market, nil
}

func (db *DB) SaveMarket(market *model.Market) error {
	data, err := json.Marshal(market)
	if err != nil {
		return fmt.Errorf("failed to marshal market: %w", err)
	}
	_, err = db.Exec(
		`INSERT INTO markets(data, updated)
                VALUES (json(:data), :updated)
         ON CONFLICT DO UPDATE SET data = :data, updated = :updated
		                       WHERE data->>'symbol' = :symbol;`,
		sql.Named("data", data),
		sql.Named("symbol", market.Symbol),
		sql.Named("updated", time.Now()),
	)
	if err != nil {
		return fmt.Errorf("failed to exec: %w", err)
	}
	return nil
}
