import db from './db.js';

const getSystemStatement = db.prepare(`SELECT data FROM systems WHERE data->>'symbol' = ?;`);
const getSystemUpdatedStatement = db.prepare(`SELECT updated FROM systems WHERE data->>'symbol' = ?;`);
const initStatement = db.prepare(`INSERT INTO config(key, value) VALUES ('systems_initialized', TRUE);`);
const isInitStatement = db.prepare(`SELECT value FROM config WHERE key = 'systems_initialized'`);
const setSystemStatement = db.prepare(`INSERT INTO systems(data) VALUES (json(?));`);
const setSystemWaypointsStatement = db.prepare(`UPDATE systems SET data = (SELECT json_set(data, '$.waypoints', json(:waypoints)) FROM systems WHERE data->>'symbol' = :symbol), updated = :date WHERE data->>'symbol' = :symbol;`);

export function init() {
	try {
		return initStatement.run().lastInsertRowid;
	} catch (err) {
		return null;
	}
}

export function isInit() {
	try {
		return isInitStatement.get().value === '1';
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
		}).changes;
	} catch (err) {
		console.log(err);
		return null;
	}
}
