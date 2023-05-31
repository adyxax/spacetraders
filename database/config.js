import db from './db.js';

const getTokenStatement = db.prepare(`SELECT json_extract(value, '$.token') as token from config where key = 'register_data';`);
const registerAgentStatement = db.prepare(`INSERT INTO config(key, value) VALUES ('register_data', json(?));`);

export function getToken() {
	try {
		return getTokenStatement.get().token;
	} catch (err) {
		console.log(err);
		return null;
	}
}

export function registerAgent(data) {
	try {
		registerAgentStatement.run(JSON.stringify(data));
		return true;
	} catch (err) {
		console.log(err);
		return false;
	}
}
