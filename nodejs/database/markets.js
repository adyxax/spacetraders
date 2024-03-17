import db from './db.js';
import * as utils from '../lib/utils.js';

const addMarketStatement = db.prepare(`INSERT INTO markets(system, data) VALUES (?, json(?));`);
const getMarketAtWaypointStatement = db.prepare(`SELECT data FROM markets WHERE data->>'symbol' = ?;`);
const getMarketsInSystemStatement = db.prepare(`SELECT data FROM markets WHERE system = ?;`);
const updateMarketStatement = db.prepare(`UPDATE markets SET data = json(:data) WHERE data->>'symbol' = :symbol;`);

export function getMarketAtWaypoint(symbol) {
    const data = getMarketAtWaypointStatement.get(symbol);
    if (data === undefined) {
	return null;
    }
    return JSON.parse(data.data);
}

export function getMarketsInSystem(symbol) {
    const data = getMarketsInSystemStatement.get(symbol);
    if (data === undefined) {
	return null;
    }
    return JSON.parse(data.data);
}

export function setMarket(data) {
    if (getMarketAtWaypoint(data.symbol) === null) {
	const system = utils.systemFromWaypoint(data.symbol);
	return addMarketStatement.run(system, JSON.stringify(data)).lastInsertRowid;
    }
    return updateMarketStatement.run({
	data: JSON.stringify(data),
	symbol: data.symbol,
    }).changes;
}
