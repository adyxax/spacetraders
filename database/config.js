import db from './db.js';

const getTokenStatement = db.prepare(`SELECT value from config where key = 'token';`);
const registerAgentStatement = db.prepare(`INSERT INTO config(key, value) VALUES ('symbol', ?), ('faction', ?), ('token', ?);`);

export function getToken() {
	try {
		return getTokenStatement.get().value;
	} catch (err) {
		console.log(err);
		return null;
	}
}

export function registerAgent(symbol, faction, token) {
	try {
		registerAgentStatement.run(symbol, faction, token);
		return true;
	} catch (err) {
		console.log(err);
		return false;
	}
}
