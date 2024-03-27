import { DbData, db } from './db.ts';

const addTokenStatement = db.prepare(`INSERT INTO tokens(data) VALUES (?);`);
const getTokenStatement = db.prepare(`SELECT data FROM tokens;`);

export function addToken(token: string): void {
	addTokenStatement.run(token);
}

export function getToken(): string|null {
	const data = getTokenStatement.get() as DbData|undefined;
	if (data === undefined) return null;
	return data.data;
}
