import { DbData, db } from './db.ts';
import { System, Waypoint } from '../lib/types.ts';

const addSystemStatement = db.prepare(`INSERT INTO systems(data) VALUES (json(?));`);
const getSystemStatement = db.prepare(`SELECT data FROM systems WHERE data->>'symbol' = ?;`);
const getSystemUpdatedStatement = db.prepare(`SELECT updated FROM systems WHERE data->>'symbol' = ?;`);
const getSystemsCountStatement = db.prepare(`SELECT COUNT(data) as data FROM systems;`);
const setSystemStatement = db.prepare(`UPDATE systems SET data = json(:data), updated = :date WHERE data->>'symbol' = :symbol;`);
const setSystemWaypointsStatement = db.prepare(`UPDATE systems SET data = (SELECT json_set(data, '$.waypoints', json(:waypoints)) FROM systems WHERE data->>'symbol' = :symbol), updated = :date WHERE data->>'symbol' = :symbol;`);

export function addSystem(data: System): void {
	addSystemStatement.run(JSON.stringify(data)).lastInsertRowid;
}

export function getSystem(symbol: string): System|null {
	const data = getSystemStatement.get(symbol) as DbData|undefined;
	if (!data) return null;
	return JSON.parse(data.data);
}

export function getSystemsCount(): number {
	const data = getSystemsCountStatement.get() as number|undefined;
	if (!data) return 0;
	return data;
}

export function getSystemUpdated(symbol: string): Date|null {
	const data = getSystemUpdatedStatement.get(symbol) as {updated: Date}|undefined;
	if (!data) return null;
	return data.updated;
}

export function setSystem(data: System): void {
	if (getSystem(data.symbol) === null) {
		addSystem(data);
	} else {
		setSystemStatement.run({
			data: JSON.stringify(data),
			date: new Date().toISOString(),
			symbol: data.symbol,
		});
	}
}

export function setSystemWaypoints(symbol: string, waypoints: Array<Waypoint>): void {
	setSystemWaypointsStatement.run({
		date: new Date().toISOString(),
		symbol: symbol,
		waypoints: JSON.stringify(waypoints),
	});
}
