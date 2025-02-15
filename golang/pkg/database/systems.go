package database

import (
	"encoding/json"
	"fmt"
	"time"

	"git.adyxax.org/adyxax/spacetraders/golang/pkg/model"
)

// ----- System -----------------------------------------------------------------
func (db *DB) LoadSystem(symbol string) (*model.System, error) {
	var buf []byte
	if err := db.QueryRow(`SELECT data FROM systems WHERE data->>'symbol' = ?;`, symbol).Scan(&buf); err != nil {
		return nil, fmt.Errorf("failed to query system: %w", err)
	}
	var system model.System
	if err := json.Unmarshal(buf, &system); err != nil {
		return nil, fmt.Errorf("failed to unmarshal system: %w", err)
	}
	return &system, nil
}

func (db *DB) SaveSystem(system *model.System) error {
	data, err := json.Marshal(system)
	if err != nil {
		return fmt.Errorf("failed to marshal system: %w", err)
	}
	if _, err := db.Exec(`INSERT INTO systems(data) VALUES (json(?));`, data); err != nil {
		return fmt.Errorf("failed to append system: %w", err)
	}
	return nil
}

// ----- Waypoint ---------------------------------------------------------------
func (db *DB) LoadWaypoint(symbol string) (*model.Waypoint, error) {
	var buf []byte
	if err := db.QueryRow(`SELECT data FROM waypoints WHERE data->>'symbol' = ?;`, symbol).Scan(&buf); err != nil {
		return nil, fmt.Errorf("failed to query waypoint: %w", err)
	}
	var waypoint model.Waypoint
	if err := json.Unmarshal(buf, &waypoint); err != nil {
		return nil, fmt.Errorf("failed to unmarshal waypoint: %w", err)
	}
	return &waypoint, nil
}

func (db *DB) SaveWaypoint(waypoint *model.Waypoint) error {
	data, err := json.Marshal(waypoint)
	if err != nil {
		return fmt.Errorf("failed to marshal waypoint: %w", err)
	}
	if _, err := db.Exec(`INSERT INTO waypoints(data, updated) VALUES (json(?), ?);`, data, time.Now()); err != nil {
		return fmt.Errorf("failed to append waypoint: %w", err)
	}
	return nil
}
