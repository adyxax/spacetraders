import * as api from './api.js';
import * as dbConfig from '../database/config.js';
import * as dbShips from '../database/ships.js';

export async function extract(ctx) {
	// TODO check if our current waypoint has an asteroid field?
	await orbit(ctx);
	const ship = dbShips.getShip(ctx.symbol);
	const response = await api.send({endpoint: `/my/ships/${ctx.symbol}/extract`, method: 'POST'});
	if (response.error !== undefined) {
		switch(response.error.code) {
		case 4000: // ship is on cooldown
			await api.sleep(response.error.data.cooldown.remainingSeconds  * 1000);
			return extract(ctx);
		case 4228: // ship is full
			return null;
		default: // yet unhandled error
			throw response;
		}
	} else {
		dbShips.setShipCargo(ctx.symbol, response.data.cargo);
		await api.sleep(response.data.cooldown.remainingSeconds*1000);
	}
	return response;
}

export async function dock(ctx) {
	const ship = dbShips.getShip(ctx.symbol);
	if (ship.nav.status === 'DOCKED') {
		return null;
	}
	const response = await api.send({endpoint: `/my/ships/${ctx.symbol}/dock`, method: 'POST'});
	if (response.error !== undefined) {
		switch(response.error.code) {
		case 4214: // ship is in transit
			await api.sleep(response.error.data.secondsToArrival  * 1000);
			return dock(ctx);
		default: // yet unhandled error
			throw response;
		}
	}
	dbShips.setShipNav(ctx.symbol, response.data.nav);
	return response;
}

export async function jump(ctx) {
	// TODO
	const response = await api.send({endpoint: `/my/ships/${ctx.ship}/jump`, method: 'POST', payload: { systemSymbol: ctx.system }});
	await api.sleep(response.data.cooldown.remainingSeconds*1000);
	return response;
}

export async function navigate(ctx) {
	const ship = dbShips.getShip(ctx.symbol);
	if (ship.nav.waypointSymbol === ctx.waypoint) {
		return null;
	}
	await orbit(ctx);
	const response = await api.send({endpoint: `/my/ships/${ctx.symbol}/navigate`, method: 'POST', payload: { waypointSymbol: ctx.waypoint }});
	dbShips.setShipFuel(ctx.symbol, response.data.fuel);
	dbShips.setShipNav(ctx.symbol, response.data.nav);
	const delay = new Date(response.data.nav.route.arrival) - new Date();
	await api.sleep(delay);
	response.data.nav.status = 'IN_ORBIT';
	dbShips.setShipNav(ctx.symbol, response.data.nav);
	return response;
}

export async function negotiate(ctx) {
	// TODO
	return await api.send({endpoint: `/my/ships/${ctx.ship}/negotiate/contract`, method: 'POST'});
}

export async function orbit(ctx) {
	const ship = dbShips.getShip(ctx.symbol);
	if (ship.nav.status === 'IN_ORBIT') {
		return null;
	}
	const response = await api.send({endpoint: `/my/ships/${ctx.symbol}/orbit`, method: 'POST'});
	if (response.error !== undefined) {
		switch(response.error.code) {
		case 4214: // ship is in transit
			await api.sleep(response.error.data.secondsToArrival  * 1000);
			return orbit(ctx);
		default: // yet unhandled error
			throw response;
		}
	}
	dbShips.setShipNav(ctx.symbol, response.data.nav);
	return response;
}

export async function purchase(ctx) {
	const response = await api.send({endpoint: '/my/ships', method: 'POST', payload: {
		shipType: ctx.shipType,
		waypointSymbol: ctx.waypoint,
	}});
	if (response.error !== undefined) {
		throw response;
	}
	dbShips.setShip(response.data.ship);
}

export async function refuel(ctx) {
	// TODO check if our current waypoint has a marketplace (and sells fuel)?
	const ship = dbShips.getShip(ctx.symbol);
	if (ship.fuel.current >= ship.fuel.capacity * 0.9) {
		return null;
	}
	await dock(ctx);
	const response = await api.send({endpoint: `/my/ships/${ctx.symbol}/refuel`, method: 'POST'});
	if (response.error !== undefined) {
		throw response;
	}
	dbShips.setShipFuel(ctx.symbol, response.data.fuel);
	// TODO track credits
	return response;
}

export async function sell(ctx) {
	await dock(ctx);
	const ship = dbShips.getShip(ctx.symbol);
	const response = await api.send({endpoint: `/my/ships/${ctx.symbol}/sell`, method: 'POST', payload: { symbol: ctx.good, units: ctx.units }});
	if (response.error !== undefined) {
		throw response;
	}
	dbShips.setShipCargo(ctx.symbol, response.data.cargo);
	// TODO track credits
	return response;
}

export async function ship(ctx) {
	const response = await api.send({endpoint: `/my/ships/${ctx.symbol}`});
	if (response.error !== undefined) {
		throw response;
	}
	dbShips.setShip(response.data);
	return response;
}

export async function survey(ctx) {
	// TODO
	return await api.send({endpoint: `/my/ships/${ctx.symbol}/survey`, method: 'POST'});
}
