import { Response } from '../model/api.ts';
import { Agent } from '../model/agent.ts';
import { Cargo } from '../model/cargo.ts';
import { Cooldown, Fuel, Nav, Ship } from '../model/ship.ts';
import * as api from './api.ts';
import * as dbAgents from '../database/agents.ts';
import * as dbShips from '../database/ships.ts';
//import * as dbSurveys from '../database/surveys.ts';
import * as systems from '../lib/systems.ts';

export async function dock(ship: Ship): Promise<void> {
	ship = dbShips.getShip(ship.symbol);
	if (ship.nav.status === 'DOCKED') return;
	const response = await api.send<{nav: Nav}>({endpoint: `/my/ships/${ship.symbol}/dock`, method: 'POST'});
	if (response.error) {
		switch(response.error.code) {
			case 4214: // ship is in transit
				const errorData = response.error.data as { secondsToArrival: number};
				await api.sleep(errorData.secondsToArrival * 1000);
				return await dock(ship);
			default: // yet unhandled error
				api.debugLog(response);
				throw response;
		}
	}
	dbShips.setShipNav(ship.symbol, response.data.nav);
}

export async function extract(ship: Ship): Promise<Cargo> {
	ship = dbShips.getShip(ship.symbol);
	if (isFull(ship)) return ship.cargo;
	// TODO move to a suitable asteroid?
	// const asteroidFields = await systems.type({symbol: ship.nav.systemSymbol, type: 'ENGINEERED_ASTEROID'});
	// TODO if there are multiple fields, find the closest one?
	//await navigate({symbol: ctx.symbol, waypoint: asteroidFields[0].symbol});
	await orbit(ship);
	// TODO handle surveying?
	const response = await api.send<{cooldown: Cooldown, cargo: Cargo}>({endpoint: `/my/ships/${ship.symbol}/extract`, method: 'POST'}); // TODO extraction and events api response fields cf https://spacetraders.stoplight.io/docs/spacetraders/b3931d097608d-extract-resources
	if (response.error) {
		switch(response.error.code) {
			case 4000: // ship is on cooldown
				const errorData = response.error.data as {cooldown: Cooldown};
				await api.sleep(errorData.cooldown.remainingSeconds  * 1000);
				return await extract(ship);
			case 4228: // ship is full
				return ship.cargo;
			default: // yet unhandled error
				api.debugLog(response);
				throw response;
		}
	} else {
		dbShips.setShipCargo(ship.symbol, response.data.cargo);
		await api.sleep(response.data.cooldown.remainingSeconds*1000);
	}
	return response.data.cargo
}

export function isFull(ship: Ship): boolean {
	ship = dbShips.getShip(ship.symbol);
	return ship.cargo.units >= ship.cargo.capacity * 0.9;
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

export async function navigate(ship: Ship, waypoint: string): Promise<void> {
	ship = dbShips.getShip(ship.symbol);
	if (ship.nav.waypointSymbol === waypoint) return;
	await orbit(ship);
	// TODO if we do not have enough fuel, make a stop to refuel along the way or drift to the destination
	const response = await api.send<{fuel: Fuel, nav: Nav}>({endpoint: `/my/ships/${ship.symbol}/navigate`, method: 'POST', payload: { waypointSymbol: waypoint }}); // TODO events field
	if (response.error) {
		switch(response.error.code) {
			case 4214: // ship is in transit
				const errorData = response.error.data as { secondsToArrival: number};
				await api.sleep(errorData.secondsToArrival * 1000);
				return await navigate(ship, waypoint);
			default: // yet unhandled error
				api.debugLog(response);
				throw response;
		}
	}
	dbShips.setShipFuel(ship.symbol, response.data.fuel);
	dbShips.setShipNav(ship.symbol, response.data.nav);
	const delay = new Date(response.data.nav.route.arrival).getTime()  - new Date().getTime() ;
	await api.sleep(delay);
	response.data.nav.status = 'IN_ORBIT'; // we arrive in orbit
	dbShips.setShipNav(ship.symbol, response.data.nav);
	// TODO only refuel at the start of a journey, if we do not have enough OR if the destination does not sell fuel?
	await refuel(ship);
}

//export async function negotiate(ctx) {
//	// TODO
//	return await api.send({endpoint: `/my/ships/${ctx.ship}/negotiate/contract`, method: 'POST'});
//}

export async function orbit(ship: Ship): Promise<void> {
	ship = dbShips.getShip(ship.symbol);
	if (ship.nav.status === 'IN_ORBIT') return;
	const response = await api.send<{nav: Nav}>({endpoint: `/my/ships/${ship.symbol}/orbit`, method: 'POST'});
	if (response.error) {
		switch(response.error.code) {
			case 4214: // ship is in transit
				const errorData = response.error.data as { secondsToArrival: number};
				await api.sleep(errorData.secondsToArrival * 1000);
				return await orbit(ship);
			default: // yet unhandled error
				throw response;
		}
	}
	dbShips.setShipNav(ship.symbol, response.data.nav);
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

export async function refuel(ship: Ship): Promise<void> {
	ship = dbShips.getShip(ship.symbol);
	if (ship.fuel.current >= ship.fuel.capacity * 0.9) return;
	// TODO check if our current waypoint has a marketplace (and sells fuel)?
	await dock(ship);
	const response = await api.send<{agent: Agent, fuel: Fuel}>({endpoint: `/my/ships/${ship.symbol}/refuel`, method: 'POST'}); // TODO transaction field
	if (response.error) {
		api.debugLog(response);
		throw response;
	}
	dbShips.setShipFuel(ship.symbol, response.data.fuel);
	dbAgents.setAgent(response.data.agent);
}

export async function sell(ship: Ship, tradeSymbol: string): Promise<Cargo> {
	ship = dbShips.getShip(ship.symbol);
	// TODO check if our current waypoint has a marketplace and buys tradeSymbol?
	await dock(ship);
	let units = 0;
	ship.cargo.inventory.forEach(i => {if (i.symbol === tradeSymbol) units = i.units; });
	const response = await api.send<{agent: Agent, cargo: Cargo}>({endpoint: `/my/ships/${ship.symbol}/sell`, method: 'POST', payload: { symbol: tradeSymbol, units: units }}); // TODO transaction field
	if (response.error) {
		api.debugLog(response);
		throw response;
	}
	dbShips.setShipCargo(ship.symbol, response.data.cargo);
	dbAgents.setAgent(response.data.agent);
	return response.data.cargo;
}

export async function ships(): Promise<Array<Ship>> {
	const response = await api.send<Array<Ship>>({endpoint: `/my/ships`, page: 1});
	if (response.error) {
		api.debugLog(response);
		throw response;
	}
	response.data.forEach(ship => dbShips.setShip(ship));
	return response.data;
}

export async function ship(ship: Ship): Promise<Ship> {
	const response = await api.send<Ship>({endpoint: `/my/ships/${ship.symbol}`});
	if (response.error) {
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
