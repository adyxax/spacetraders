package database

import (
	"context"
	"database/sql"
	"embed"
	"io/fs"

	_ "github.com/mattn/go-sqlite3"
)

type DB struct {
	ctx context.Context
	db  *sql.DB
}

//go:embed sql/*.sql
var schemaFiles embed.FS

func DBInit(ctx context.Context, url string) (myDB *DB, err error) {
	var db *sql.DB
	if db, err = sql.Open("sqlite3", url); err != nil {
		return nil, err
	}
	defer func() {
		if err != nil {
			_ = db.Close()
		}
	}()

	if _, err = db.ExecContext(ctx, "PRAGMA foreign_keys = ON"); err != nil {
		return nil, err
	}
	if _, err = db.ExecContext(ctx, "PRAGMA journal_mode = WAL"); err != nil {
		return nil, err
	}

	var version int
	if err = db.QueryRowContext(ctx, `SELECT version FROM schema_version;`).Scan(&version); err != nil {
		if err.Error() == "no such table: schema_version" {
			version = 0
		} else {
			return nil, err
		}
	}

	statements := make([]string, 0)
	err = fs.WalkDir(schemaFiles, ".", func(path string, d fs.DirEntry, err error) error {
		if d.IsDir() || err != nil {
			return err
		}
		var stmts []byte
		if stmts, err = schemaFiles.ReadFile(path); err != nil {
			return err
		} else {
			statements = append(statements, string(stmts))
		}
		return nil
	})
	if err != nil {
		return nil, err
	}

	tx, err := db.Begin()
	if err != nil {
		return nil, err
	}
	for version < len(statements) {
		if _, err = tx.ExecContext(ctx, statements[version]); err != nil {
			tx.Rollback()
			return nil, err
		}
		version++
	}
	if _, err = tx.ExecContext(ctx, `DELETE FROM schema_version; INSERT INTO schema_version (version) VALUES (?);`, version); err != nil {
		tx.Rollback()
		return nil, err
	}
	tx.Commit()
	return &DB{ctx: ctx, db: db}, nil
}

func (db *DB) Close() error {
	return db.db.Close()
}
