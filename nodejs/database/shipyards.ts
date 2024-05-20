import { DbData, db } from './db.ts';
import { Shipyard } from '../lib/types';

const addStatement = db.prepare(`INSERT INTO shipyards(data, updated) VALUES (json(:data), :date);`);
const getStatement = db.prepare(`SELECT data FROM shipyards WHERE data->>'symbol' = ?;`);
const updateStatement = db.prepare(`UPDATE shipyards SET data = json(:data), updated = :date WHERE data->>'symbol' = :symbol;`);

export function get(symbol: string): Shipyard|null {
	const data = getStatement.get(symbol) as DbData|undefined;
	if (!data) return null;
	return JSON.parse(data.data);
}

export function set(data: Shipyard): void {
    if (get(data.symbol) === null) {
		addStatement.run({
			data: JSON.stringify(data),
			date: new Date().toISOString(),
		});
    } else {
		updateStatement.run({
			data: JSON.stringify(data),
			date: new Date().toISOString(),
			symbol: data.symbol,
		});
	}
}
