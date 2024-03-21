import db from './db.ts';
import { Cargo } from '../model/cargo.ts';
import { Fuel, Nav, Ship } from '../model/ship.ts';

const addShipStatement = db.prepare(`INSERT INTO ships(data) VALUES (json(?));`);
const getShipStatement = db.prepare(`SELECT data FROM ships WHERE data->>'symbol' = ?;`);
const getShipsAtStatement = db.prepare(`SELECT data FROM ships WHERE data->>'$.nav.systemSymbol' = ?;`);
const setShipCargoStatement = db.prepare(`UPDATE ships SET data = (SELECT json_set(data, '$.cargo', json(:cargo)) FROM ships WHERE data->>'symbol' = :symbol) WHERE data->>'symbol' = :symbol;`);
const setShipFuelStatement = db.prepare(`UPDATE ships SET data = (SELECT json_set(data, '$.fuel', json(:fuel)) FROM ships WHERE data->>'symbol' = :symbol) WHERE data->>'symbol' = :symbol;`);
const setShipNavStatement = db.prepare(`UPDATE ships SET data = (SELECT json_set(data, '$.nav', json(:nav)) FROM ships WHERE data->>'symbol' = :symbol) WHERE data->>'symbol' = :symbol;`);
const updateShipStatement = db.prepare(`UPDATE ships SET data = json(:data) WHERE data->>'symbol' = :symbol;`);

export function getShip(symbol: string): Ship|null {
	const data = getShipStatement.get(symbol) as {data: string}|undefined;
	if (!data) return null;
	return JSON.parse(data.data);
}

export function getShipsAt(symbol: string) {
	const data = getShipsAtStatement.all(symbol) as Array<{data: string}>;
	return data.map(elt => JSON.parse(elt.data));
}


export function setShip(data: Ship) {
	if (getShip(data.symbol) === null) {
		addShipStatement.run(JSON.stringify(data));
	} else {
		updateShipStatement.run({
			data: JSON.stringify(data),
			symbol: data.symbol,
		});
	}
}

export function setShipCargo(symbol: string, cargo: Cargo) {
	setShipCargoStatement.run({
		cargo: JSON.stringify(cargo),
		symbol: symbol,
	});
}

export function setShipFuel(symbol: string, fuel: Fuel) {
	setShipFuelStatement.run({
		fuel: JSON.stringify(fuel),
		symbol: symbol,
	});
}

export function setShipNav(symbol: string, nav: Nav) {
	setShipNavStatement.run({
		nav: JSON.stringify(nav),
		symbol: symbol,
	});
}
