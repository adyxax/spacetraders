package database

import (
	"context"
	"database/sql"
	"fmt"
	"runtime"
	"strings"
)

func initDB(ctx context.Context, url string) (*sql.DB, error) {
	db, err := sql.Open("sqlite3", url)
	if err != nil {
		return nil, fmt.Errorf("failed to open database: %w", err)
	}
	defer func() {
		if err != nil {
			_ = db.Close()
		}
	}()
	if _, err = db.ExecContext(ctx, "PRAGMA busy_timeout = 5000"); err != nil {
		return nil, fmt.Errorf("failed to set pragma: %w", err)
	}

	return db, nil
}

type DB struct {
	ctx     context.Context
	readDB  *sql.DB
	writeDB *sql.DB
}

func NewDB(ctx context.Context, url string) (*DB, error) {
	readDB, err := initDB(ctx, url)
	if err != nil {
		return nil, fmt.Errorf("failed to init read database connection: %w", err)
	}
	defer func() {
		if err != nil {
			_ = readDB.Close()
		}
	}()
	readDB.SetMaxOpenConns(max(4, runtime.NumCPU()))

	writeDB, err := initDB(ctx, url)
	if err != nil {
		return nil, fmt.Errorf("failed to init write database connection: %w", err)
	}
	defer func() {
		if err != nil {
			_ = writeDB.Close()
		}
	}()
	writeDB.SetMaxOpenConns(1)

	db := DB{
		ctx:     ctx,
		readDB:  readDB,
		writeDB: writeDB,
	}
	pragmas := []struct {
		key   string
		value string
	}{
		{"foreign_keys", "ON"},
		{"cache_size", "10000000"},
		{"journal_mode", "WAL"},
		{"synchronous", "NORMAL"},
	}
	for _, pragma := range pragmas {
		if _, err = db.Exec(fmt.Sprintf("PRAGMA %s = %s", pragma.key, pragma.value)); err != nil {
			return nil, fmt.Errorf("failed to set pragma: %w", err)
		}
	}
	if err = db.migrate(); err != nil {
		return nil, fmt.Errorf("failed to migrate: %w", err)
	}

	return &db, nil
}

func (db *DB) Close() error {
	if err := db.readDB.Close(); err != nil {
		_ = db.writeDB.Close()
		return fmt.Errorf("failed to close read database connection: %w", err)
	}
	if err := db.writeDB.Close(); err != nil {
		return fmt.Errorf("failed to close write database connection: %w", err)
	}
	return nil
}

func (db *DB) Reset() error {
	_, err := db.Exec(strings.Join([]string{
		"DELETE FROM agents;",
		"DELETE FROM markets;",
		"DELETE FROM shipyards;",
		"DELETE FROM systems;",
		"DELETE FROM tokens;",
		"DELETE FROM transactions;",
		"DELETE FROM waypoints;",
	}, ""))
	if err != nil {
		return fmt.Errorf("failed to reset database: %w", err)
	}
	return nil
}

func (db *DB) Exec(query string, args ...any) (sql.Result, error) {
	return db.writeDB.ExecContext(db.ctx, query, args...)
}

func (db *DB) Query(query string, args ...any) (*sql.Rows, error) {
	return db.readDB.QueryContext(db.ctx, query, args...)
}

func (db *DB) QueryRow(query string, args ...any) *sql.Row {
	return db.readDB.QueryRowContext(db.ctx, query, args...)
}

func (db *DB) WithTransaction(f func(tx *sql.Tx) error) error {
	tx, err := db.writeDB.Begin()
	if err != nil {
		return fmt.Errorf("failed to begin transaction: %w", err)
	}
	defer func() {
		if err != nil {
			if err2 := tx.Rollback(); err2 != nil {
				panic(fmt.Sprintf("failed to rollback transaction: %+v. Reason for rollback: %+v", err2, err))
			}
		}
	}()
	if err = f(tx); err != nil {
		return fmt.Errorf("failed to execute function inside transaction: %w", err)
	} else {
		if err = tx.Commit(); err != nil {
			err = fmt.Errorf("failed to commit transaction: %w", err)
		}
	}
	return err
}
