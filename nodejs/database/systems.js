import db from './db.js';

const addSystemStatement = db.prepare(`INSERT INTO systems(data) VALUES (json(?));`);
const getSystemStatement = db.prepare(`SELECT data FROM systems WHERE data->>'symbol' = ?;`);
const getSystemUpdatedStatement = db.prepare(`SELECT updated FROM systems WHERE data->>'symbol' = ?;`);
const getSystemsCountStatement = db.prepare(`SELECT COUNT(data) as data FROM systems;`);
const setSystemWaypointsStatement = db.prepare(`UPDATE systems SET data = (SELECT json_set(data, '$.waypoints', json(:waypoints)) FROM systems WHERE data->>'symbol' = :symbol), updated = :date WHERE data->>'symbol' = :symbol;`);

export function addSystem(data) {
	return addSystemStatement.run(JSON.stringify(data)).lastInsertRowid;
}

export function getSystem(symbol) {
	const data = getSystemStatement.get(symbol);
	if (data === undefined) {
		return null;
	}
	return JSON.parse(data.data);
}

export function getSystemsCount() {
	const data = getSystemsCountStatement.get();
	if (data === undefined) {
		return null;
	}
	return data.data;
}

export function getSystemUpdated(symbol) {
	const updated = getSystemUpdatedStatement.get(symbol);
	if (updated === undefined) {
		return null;
	}
	return updated.updated;
}

export function setSystemWaypoints(symbol, waypoints) {
	return setSystemWaypointsStatement.run({
		date: new Date().toISOString(),
		symbol: symbol,
		waypoints: JSON.stringify(waypoints),
	}).changes;
}
