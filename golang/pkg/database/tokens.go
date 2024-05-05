package database

func (db DB) AddToken(token string) error {
	_, err := db.db.ExecContext(db.ctx, `INSERT INTO tokens(data) VALUES (?);`, token)
	return err
}

func (db DB) GetToken() (string, error) {
	var token string
	if err := db.db.QueryRowContext(db.ctx, `SELECT data FROM tokens;`).Scan(&token); err != nil {
		return "", err
	}
	return token, nil
}
