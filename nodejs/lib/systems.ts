import * as api from './api.ts';
import * as dbMarkets from '../database/markets.ts';
import * as dbShips from '../database/ships.ts';
import * as dbSystems from '../database/systems.ts';
import { Market } from '../model/market.ts'
import { System, Waypoint } from '../model/system.ts'
import * as utils from './utils.ts';

export async function market(waypointSymbol: string): Promise<Market> {
    const data = dbMarkets.getMarketAtWaypoint(waypointSymbol);
	if (data) { return data; }
	const systemSymbol = utils.systemFromWaypoint(waypointSymbol);
	let response = await api.send<Market>({endpoint: `/systems/${systemSymbol}/waypoints/${waypointSymbol}/market`});
	if (response.error) {
		api.debugLog(response);
		throw response;
	}
	dbMarkets.setMarket(response.data);
	return response.data;
}

//export async function shipyard(waypoint: string): Promise<unknown> {
//	// TODO database caching
//	const systemSymbol = utils.systemFromWaypoint(waypoint);
//	return await api.send({endpoint: `/systems/${systemSymbol}/waypoints/${waypoint}/shipyard`});
//}

export async function system(symbol: string): Promise<System> {
	let data = dbSystems.getSystem(symbol);
	if (data) { return data; }
	const response = await api.send<System>({endpoint: `/systems/${symbol}`});
	if (response.error) {
		api.debugLog(response);
		throw response;
	}
	dbSystems.setSystem(response.data);
	return response.data;
}

// Retrieves a list of waypoints that have a specific trait like a SHIPYARD or a MARKETPLACE
export async function trait(system: string, trait: string): Promise<Array<Waypoint>> {
	const ws = await waypoints(system);
	return ws.filter(w => w.traits.some(t => t.symbol === trait));
}

// Retrieves a list of waypoints that have a specific type like ASTEROID_FIELD
export async function type(system: string, typeSymbol: string): Promise<Array<Waypoint>> {
	const ws = await waypoints(system);
	return ws.filter(s => s.type === typeSymbol);
}

export async function waypoints(systemSymbol: string): Promise<Array<Waypoint>> {
	const s = await system(systemSymbol);
	const updated = dbSystems.getSystemUpdated(systemSymbol);
	// TODO handle uncharted systems
	if (updated) return s.waypoints;
	const waypoints = await api.sendPaginated<Waypoint>({endpoint: `/systems/${systemSymbol}/waypoints`});
	dbSystems.setSystemWaypoints(systemSymbol, waypoints);
	return waypoints;
}
