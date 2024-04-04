import { DbData, db } from './db.ts';
import { Market } from '../lib/types.ts';
import { systemFromWaypoint } from '../lib/utils.ts';

const addMarketStatement = db.prepare(`INSERT INTO markets(system, data) VALUES (?, json(?));`);
const getMarketAtWaypointStatement = db.prepare(`SELECT data FROM markets WHERE data->>'symbol' = ?;`);
//const getMarketsInSystemStatement = db.prepare(`SELECT data FROM markets WHERE system = ?;`);
const updateMarketStatement = db.prepare(`UPDATE markets SET data = json(:data) WHERE data->>'symbol' = :symbol;`);

export function getMarketAtWaypoint(symbol: string): Market|null {
    const data = getMarketAtWaypointStatement.get(symbol) as DbData|undefined;
	if (!data) return null;
    return JSON.parse(data.data);
}

export function setMarket(data: Market): void {
    if (getMarketAtWaypoint(data.symbol) === null) {
		const system = systemFromWaypoint(data.symbol);
		addMarketStatement.run(system, JSON.stringify(data));
    } else {
		updateMarketStatement.run({
			data: JSON.stringify(data),
			symbol: data.symbol,
		});
	}
}
