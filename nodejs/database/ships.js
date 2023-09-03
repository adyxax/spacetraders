import db from './db.js';

const addShipStatement = db.prepare(`INSERT INTO ships(data) VALUES (json(?));`);
const getShipStatement = db.prepare(`SELECT data FROM ships WHERE data->>'symbol' = ?;`);
const setShipCargoStatement = db.prepare(`UPDATE ships SET data = (SELECT json_set(data, '$.cargo', json(:cargo)) FROM ships WHERE data->>'symbol' = :symbol) WHERE data->>'symbol' = :symbol;`);
const setShipFuelStatement = db.prepare(`UPDATE ships SET data = (SELECT json_set(data, '$.fuel', json(:fuel)) FROM ships WHERE data->>'symbol' = :symbol) WHERE data->>'symbol' = :symbol;`);
const setShipNavStatement = db.prepare(`UPDATE ships SET data = (SELECT json_set(data, '$.nav', json(:nav)) FROM ships WHERE data->>'symbol' = :symbol) WHERE data->>'symbol' = :symbol;`);
const updateShipStatement = db.prepare(`UPDATE ships SET data = json(:data) WHERE data->>'symbol' = :symbol;`);

export function getShip(symbol) {
	const data = getShipStatement.get(symbol);
	if (data === undefined) {
		return null;
	}
	return JSON.parse(data.data);
}

export function setShip(data) {
	if (getShip(data.symbol) === null) {
		return addShipStatement.run(JSON.stringify(data)).lastInsertRowid;
	} else {
		return updateShipStatement.run({
			data: JSON.stringify(data),
			symbol: data.symbol,
		}).changes;
	}
}

export function setShipCargo(symbol, cargo) {
	return setShipCargoStatement.run({
		cargo: JSON.stringify(cargo),
		symbol: symbol,
	}).changes;
}

export function setShipFuel(symbol, fuel) {
	return setShipFuelStatement.run({
		fuel: JSON.stringify(fuel),
		symbol: symbol,
	}).changes;
}

export function setShipNav(symbol, nav) {
	return setShipNavStatement.run({
		nav: JSON.stringify(nav),
		symbol: symbol,
	}).changes;
}
