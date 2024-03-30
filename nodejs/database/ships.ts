import { DbData, db } from './db.ts';
import { Cargo } from '../model/cargo.ts';
import { Fuel, Nav, Ship } from '../model/ship.ts';

const addShipStatement = db.prepare(`INSERT INTO ships(data) VALUES (json(?));`);
const getShipStatement = db.prepare(`SELECT data FROM ships WHERE data->>'symbol' = ?;`);
const getShipsAtStatement = db.prepare(`SELECT data FROM ships WHERE data->>'$.nav.systemSymbol' = ?;`);
const setShipCargoStatement = db.prepare(`UPDATE ships SET data = (SELECT json_set(data, '$.cargo', json(:cargo)) FROM ships WHERE data->>'symbol' = :symbol) WHERE data->>'symbol' = :symbol;`);
const setShipFuelStatement = db.prepare(`UPDATE ships SET data = (SELECT json_set(data, '$.fuel', json(:fuel)) FROM ships WHERE data->>'symbol' = :symbol) WHERE data->>'symbol' = :symbol;`);
const setShipNavStatement = db.prepare(`UPDATE ships SET data = (SELECT json_set(data, '$.nav', json(:nav)) FROM ships WHERE data->>'symbol' = :symbol) WHERE data->>'symbol' = :symbol;`);
const updateShipStatement = db.prepare(`UPDATE ships SET data = json(:data) WHERE data->>'symbol' = :symbol;`);

export function getShip(symbol: string): Ship {
	const data = getShipStatement.get(symbol) as DbData|undefined;
	if (!data) throw `invalid symbol ${symbol} in getShip database call`;
	return JSON.parse(data.data);
}

export function getShipsAt(symbol: string): Array<Ship> {
	const data = getShipsAtStatement.all(symbol) as Array<DbData>;
	return data.map(elt => JSON.parse(elt.data));
}


export function setShip(data: Ship): void {
	const changes = updateShipStatement.run({
		data: JSON.stringify(data),
		symbol: data.symbol,
	}).changes;
	if (changes === 0) addShipStatement.run(JSON.stringify(data));
}

export function setShipCargo(symbol: string, cargo: Cargo): void {
	setShipCargoStatement.run({
		cargo: JSON.stringify(cargo),
		symbol: symbol,
	});
}

export function setShipFuel(symbol: string, fuel: Fuel): void {
	setShipFuelStatement.run({
		fuel: JSON.stringify(fuel),
		symbol: symbol,
	});
}

export function setShipNav(symbol: string, nav: Nav): void {
	setShipNavStatement.run({
		nav: JSON.stringify(nav),
		symbol: symbol,
	});
}
