import db from './db.js';

const getSystemStatement = db.prepare(`SELECT data from systems where json_extract(data, '$.symbol') = ?;`);
const getSystemUpdatedStatement = db.prepare(`SELECT updated from systems where json_extract(data, '$.symbol') = ?;`);
const setSystemStatement = db.prepare(`INSERT INTO systems(data) VALUES (json(?));`);
const setSystemWaypointsStatement = db.prepare(`UPDATE systems SET data = (SELECT json_set(data, '$.waypoints', json(:waypoints)) FROM systems WHERE json_extract(data, '$.symbol') = :symbol), updated = :date WHERE json_extract(data, '$.symbol') = :symbol;`);

export function init() {
	try {
		return db.prepare(`INSERT INTO config(key, value) VALUES ('systems_initialized', TRUE);`).run().lastInsertRowid;
	} catch (err) {
		return null;
	}
}

export function isInit() {
	try {
		return db.prepare(`SELECT value FROM config WHERE key = 'systems_initialized'`).get().value === '1';
	} catch (err) {
		return false;
	}
}

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

export function getSystemUpdated(symbol) {
	try {
		const updated = getSystemUpdatedStatement.get(symbol);
		if (updated === undefined) {
			return null;
		}
		return updated.updated;
	} catch (err) {
		console.log(err);
		return null;
	}
}

export function setSystem(data) {
	try {
		return setSystemStatement.run(JSON.stringify(data)).lastInsertRowid;
	} catch (err) {
		return null;
	}
}

export function setSystemWaypoints(symbol, waypoints) {
	try {
		return setSystemWaypointsStatement.run({
			date: new Date().toISOString(),
			symbol: symbol,
			waypoints: JSON.stringify(waypoints),
		});
	} catch (err) {
		console.log(err);
		return null;
	}
}
