import { Response } from '../model/api.ts';
import { Agent } from '../model/agent.ts';
import { Cargo } from '../model/cargo.ts';
import { Cooldown, Fuel, Nav, Ship } from '../model/ship.ts';
import * as api from './api.js';
import * as dbAgents from '../database/agents.ts';
import * as dbShips from '../database/ships.js';
import * as dbSurveys from '../database/surveys.js';
import * as systems from '../lib/systems.js';

export async function dock(ship: Ship): Promise<Ship> {
	if (ship.nav.status === 'DOCKED') return ship;
	const response = await api.send({endpoint: `/my/ships/${ship.symbol}/dock`, method: 'POST'}) as Response<{nav: Nav}>;
	if (response.error !== undefined) {
		switch(response.error.code) {
			case 4214: // ship is in transit
				await api.sleep(response.error.data.secondsToArrival  * 1000);
				return await dock(ship);
			default: // yet unhandled error
				api.debugLog(response);
				throw response;
		}
	}
	dbShips.setShipNav(ship.symbol, response.data.nav);
	ship.nav = response.data.nav;
	return ship;
}

export async function extract(ship: Ship): Promise<Ship> {
	// TODO move to a suitable asteroid?
	// const asteroidFields = await systems.type({symbol: ship.nav.systemSymbol, type: 'ENGINEERED_ASTEROID'});
	// TODO if there are multiple fields, find the closest one?
	//await navigate({symbol: ctx.symbol, waypoint: asteroidFields[0].symbol});
	ship = await orbit(ship);
	// TODO handle surveying?
	const response = await api.send({endpoint: `/my/ships/${ship.symbol}/extract`, method: 'POST'}) as Response<{cooldown: Cooldown, cargo: Cargo}>; // TODO extraction and events api response fields cf https://spacetraders.stoplight.io/docs/spacetraders/b3931d097608d-extract-resources
	if (response.error !== undefined) {
		switch(response.error.code) {
			case 4000: // ship is on cooldown
				await api.sleep(response.error.data.cooldown.remainingSeconds  * 1000);
				return await extract(ship);
			case 4228: // ship is full
				return ship;
			default: // yet unhandled error
				api.debugLog(response);
				throw response;
		}
	} else {
		dbShips.setShipCargo(ship.symbol, response.data.cargo);
		ship.cargo = response.data.cargo;
		await api.sleep(response.data.cooldown.remainingSeconds*1000);
	}
	return ship;
}

//function hasMount(shipSymbol, mountSymbol) {
//	const ship = dbShips.getShip(shipSymbol);
//	return ship.mounts.filter(s => s.symbol === mountSymbol).length > 0;
//}

//export async function jump(ship: Ship): Ship {
//	// TODO
//	const response = await api.send({endpoint: `/my/ships/${ctx.ship}/jump`, method: 'POST', payload: { systemSymbol: ctx.system }});
//	await api.sleep(response.data.cooldown.remainingSeconds*1000);
//	return response;
//}

export async function navigate(ship: Ship, waypoint: string): Promise<Ship> {
	if (ship.nav.waypointSymbol === waypoint) return ship;
	ship = await orbit(ship);
	// TODO if we do not have enough fuel, make a stop to refuel along the way or drift to the destination
	const response = await api.send({endpoint: `/my/ships/${ship.symbol}/navigate`, method: 'POST', payload: { waypointSymbol: waypoint }}) as Response<{fuel: Fuel, nav: Nav}>; // TODO events field
	if (response.error !== undefined) {
		switch(response.error.code) {
			case 4214: // ship is in transit
				await api.sleep(response.error.data.secondsToArrival  * 1000);
				return await navigate(ship, waypoint);
			default: // yet unhandled error
				api.debugLog(response);
				throw response;
		}
	}
	dbShips.setShipFuel(ship.symbol, response.data.fuel);
	dbShips.setShipNav(ship.symbol, response.data.nav);
	ship.fuel = response.data.fuel;
	ship.nav = response.data.nav
	const delay = new Date(response.data.nav.route.arrival).getTime()  - new Date().getTime() ;
	await api.sleep(delay);
	response.data.nav.status = 'IN_ORBIT'; // we arrive in orbit
	dbShips.setShipNav(ship.symbol, response.data.nav);
	ship.nav = response.data.nav
	ship = await refuel(ship);
	return ship;
}

//export async function negotiate(ctx) {
//	// TODO
//	return await api.send({endpoint: `/my/ships/${ctx.ship}/negotiate/contract`, method: 'POST'});
//}

export async function orbit(ship: Ship): Promise<Ship> {
	if (ship.nav.status === 'IN_ORBIT') return ship;
	const response = await api.send({endpoint: `/my/ships/${ship.symbol}/orbit`, method: 'POST'}) as Response<{nav: Nav}>;
	if (response.error !== undefined) {
		switch(response.error.code) {
			case 4214: // ship is in transit
				await api.sleep(response.error.data.secondsToArrival  * 1000);
				return await orbit(ship);
			default: // yet unhandled error
				throw response;
		}
	}
	dbShips.setShipNav(ship.symbol, response.data.nav);
	ship.nav = response.data.nav;
	return ship;
}

//export async function purchase(ctx) {
//	const response = await api.send({endpoint: '/my/ships', method: 'POST', payload: {
//		shipType: ctx.shipType,
//		waypointSymbol: ctx.waypoint,
//	}});
//	if (response.error !== undefined) {
//		throw response;
//	}
//	dbShips.setShip(response.data.ship);
//	return response.data;
//}

export async function refuel(ship: Ship): Promise<Ship> {
	if (ship.fuel.current >= ship.fuel.capacity * 0.9) return ship;
	// TODO check if our current waypoint has a marketplace (and sells fuel)?
	ship = await dock(ship);
	const response = await api.send({endpoint: `/my/ships/${ship.symbol}/refuel`, method: 'POST'}) as Response<{agent: Agent, fuel: Fuel}>; // TODO transaction field
	if (response.error !== undefined) {
		api.debugLog(response);
		throw response;
	}
	dbShips.setShipFuel(ship.symbol, response.data.fuel);
	dbAgents.setAgent(response.data.agent);
	ship.fuel = response.data.fuel;
	return ship;
}

export async function sell(ship: Ship, tradeSymbol: string): Promise<Ship> {
	// TODO check if our current waypoint has a marketplace?
	ship = await dock(ship);
	let units = 0;
	ship.cargo.inventory.forEach(i => {if (i.symbol === tradeSymbol) units = i.units; });
	const response = await api.send({endpoint: `/my/ships/${ship.symbol}/sell`, method: 'POST', payload: { symbol: tradeSymbol, units: units }}) as Response<{agent: Agent, cargo: Cargo}>; // TODO transaction field
	if (response.error !== undefined) {
		api.debugLog(response);
		throw response;
	}
	dbShips.setShipCargo(ship.symbol, response.data.cargo);
	dbAgents.setAgent(response.data.agent);
	ship.cargo = response.data.cargo;
	return ship;
}

export async function ships(): Promise<Array<Ship>> {
	const response = await api.send({endpoint: `/my/ships`, page: 1}) as Response<Array<Ship>>;
	if (response.error !== undefined) {
		api.debugLog(response);
		throw response;
	}
	response.data.forEach(ship => dbShips.setShip(ship));
	return response.data;
}

export async function ship(ship: Ship): Promise<Ship> {
	const response = await api.send({endpoint: `/my/ships/${ship.symbol}`}) as Response<Ship>;
	if (response.error !== undefined) {
		api.debugLog(response);
		throw response;
	}
	dbShips.setShip(response.data);
	return response.data;
}

//export async function survey(ctx) {
//	if (!hasMount(ctx.symbol, 'MOUNT_SURVEYOR_I')) { // we check if a surveyor is mounted on the ship
//		return null;
//	}
//	const ship = dbShips.getShip(ctx.symbol);
//	const asteroidFields = await systems.type({symbol: ship.nav.systemSymbol, type: 'ASTEROID_FIELD'});
//	// TODO if there are multiple fields, find the closest one?
//	await navigate({symbol: ctx.symbol, waypoint: asteroidFields[0].symbol});
//	await orbit(ctx);
//	const response = await api.send({endpoint: `/my/ships/${ctx.symbol}/survey`, method: 'POST'});
//	api.debugLog(response);
//	if (response.error !== undefined) {
//		switch(response.error.code) {
//			case 4000: // ship is on cooldown
//				await api.sleep(response.error.data.cooldown.remainingSeconds  * 1000);
//				return await survey(ctx);
//			default: // yet unhandled error
//				throw response;
//		}
//	}
//	dbSurveys.set(response.data.surveys[0]);
//	await api.sleep(response.data.cooldown.remainingSeconds*1000);
//	return response;
//}
