import db from './db.js';

const addTokenStatement = db.prepare(`INSERT INTO tokens(data) VALUES (?);`);
const getTokenStatement = db.prepare(`SELECT data FROM tokens;`);

export function addToken(token) {
	return addTokenStatement.run(token).lastInsertRowid;
}

export function getToken() {
	const data = getTokenStatement.get();
	if (data === undefined) {
		return null;
	}
	return data.data;
}
