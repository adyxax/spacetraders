import db from './db.js';

const getShipStatement = db.prepare(`SELECT data FROM ships WHERE data->>'symbol' = ?;`);
const setShipStatement = db.prepare(`INSERT INTO ships(data, updated) VALUES (json(?), ?);`);
const setShipCargoStatement = db.prepare(`UPDATE ships SET data = (SELECT json_set(data, '$.cargo', json(:cargo)) FROM ships WHERE data->>'symbol' = :symbol), updated = :date WHERE data->>'symbol' = :symbol;`);
const setShipFuelStatement = db.prepare(`UPDATE ships SET data = (SELECT json_set(data, '$.fuel', json(:fuel)) FROM ships WHERE data->>'symbol' = :symbol), updated = :date WHERE data->>'symbol' = :symbol;`);
const setShipNavStatement = db.prepare(`UPDATE ships SET data = (SELECT json_set(data, '$.nav', json(:nav)) FROM ships WHERE data->>'symbol' = :symbol), updated = :date WHERE data->>'symbol' = :symbol;`);
const updateShipStatement = db.prepare(`UPDATE ships SET data = json(:data), updated = :date WHERE data->>'symbol' = :symbol;`);

export function getShip(symbol) {
	try {
		const data = getShipStatement.get(symbol);
		if (data === undefined) {
			return null;
		}
		return JSON.parse(data.data);
	} catch (err) {
		console.log(err);
		return null;
	}
}

export function setShip(data) {
	if (getShip(data.symbol) === null) {
		try {
			return setShipStatement.run(JSON.stringify(data), new Date().toISOString()).lastInsertRowid;
		} catch (err) {
			console.log(err);
			return null;
		}
	} else {
		try {
			return updateShipStatement.run({
				data: JSON.stringify(data),
				date: new Date().toISOString(),
				symbol: data.symbol,
			}).changes;
		} catch (err) {
			console.log(err);
			return null;
		}
	}
}

export function setShipCargo(symbol, cargo) {
	try {
		setShipCargoStatement.run({
			cargo: JSON.stringify(cargo),
			date: new Date().toISOString(),
			symbol: symbol,
		}).changes;
	} catch (err) {
		console.log(err);
		return null;
	}
}

export function setShipFuel(symbol, fuel) {
	try {
		setShipFuelStatement.run({
			date: new Date().toISOString(),
			fuel: JSON.stringify(fuel),
			symbol: symbol,
		}).changes;
	} catch (err) {
		console.log(err);
		return null;
	}
}

export function setShipNav(symbol, nav) {
	try {
		setShipNavStatement.run({
			date: new Date().toISOString(),
			nav: JSON.stringify(nav),
			symbol: symbol,
		}).changes;
	} catch (err) {
		console.log(err);
		return null;
	}
}
