package database

func (db *DB) AddToken(token string) error {
	_, err := db.Exec(`INSERT INTO tokens(data) VALUES (?);`, token)
	return err
}

func (db *DB) GetToken() (string, error) {
	var token string
	err := db.QueryRow(`SELECT data FROM tokens;`).Scan(&token)
	return token, err
}
