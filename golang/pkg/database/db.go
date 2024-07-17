package database

import (
	"context"
	"database/sql"
)

type DB struct {
	ctx context.Context
	db  *sql.DB
}

func (db *DB) Exec(query string, args ...any) (sql.Result, error) {
	return db.db.ExecContext(db.ctx, query, args...)
}

func (db *DB) QueryRow(query string, args ...any) *sql.Row {
	return db.db.QueryRowContext(db.ctx, query, args...)
}
