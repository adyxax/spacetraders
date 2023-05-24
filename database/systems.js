import db from './db.js';

const getSystemStatement = db.prepare(`SELECT data from systems where symbol = ?;`);
const setSystemStatement = db.prepare(`INSERT INTO systems(symbol, data) VALUES (?, ?);`);

export function getSystem(symbol) {
	try {
		const data = getSystemStatement.get(symbol);
		if (data === undefined) {
			return null;
		}
		return JSON.parse(data.data);
	} catch (err) {
		console.log(err);
		return null;
	}
}

export function setSystem(symbol, data) {
	try {
		return setSystemStatement.run(symbol, JSON.stringify(data)).lastInsertRowid;
	} catch (err) {
		console.log(err);
		return null;
	}
}
