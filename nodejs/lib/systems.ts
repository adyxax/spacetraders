import {
	debugLog,
	send,
	sendPaginated,
} from './api.ts';
import * as dbMarkets from '../database/markets.ts';
import * as dbShipyards from '../database/shipyards.ts';
import * as dbSystems from '../database/systems.ts';
import {
	Market,
	Shipyard,
	System,
	Waypoint,
} from './types.ts'
import {
	is_there_a_ship_at_this_waypoint,
	systemFromWaypoint,
} from './utils.ts';

export async function market(waypoint: Waypoint): Promise<Market> {
    const data = dbMarkets.getMarketAtWaypoint(waypoint.symbol);
	if (data && (data.tradeGoods || !is_there_a_ship_at_this_waypoint(waypoint))) { return data; }
	const systemSymbol = systemFromWaypoint(waypoint.symbol);
	let response = await send<Market>({endpoint: `/systems/${systemSymbol}/waypoints/${waypoint.symbol}/market`});
	if (response.error) {
		debugLog(response);
		throw response;
	}
	dbMarkets.setMarket(response.data);
	return response.data;
}

export async function shipyard(waypoint: Waypoint): Promise<Shipyard> {
	const data = dbShipyards.get(waypoint.symbol);
	if (data && (data.ships || !is_there_a_ship_at_this_waypoint(waypoint))) { return data; }
	const systemSymbol = systemFromWaypoint(waypoint.symbol);
	const response = await send<Shipyard>({endpoint: `/systems/${systemSymbol}/waypoints/${waypoint.symbol}/shipyard`});
	if (response.error) {
		debugLog(response);
		throw response;
	}
	dbShipyards.set(response.data);
	return response.data;
}

export async function system(symbol: string): Promise<System> {
	let data = dbSystems.getSystem(symbol);
	if (data) { return data; }
	const response = await send<System>({endpoint: `/systems/${symbol}`});
	if (response.error) {
		debugLog(response);
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

export async function waypoint(waypointSymbol: string): Promise<Waypoint> {
	const systemSymbol = systemFromWaypoint(waypointSymbol);
	const w = await waypoints(systemSymbol);
	return w.filter(w => w.symbol === waypointSymbol)[0];
}

export async function waypoints(systemSymbol: string): Promise<Array<Waypoint>> {
	const s = await system(systemSymbol);
	const updated = dbSystems.getSystemUpdated(systemSymbol);
	// TODO handle uncharted systems
	if (updated) return s.waypoints;
	const waypoints = await sendPaginated<Waypoint>({endpoint: `/systems/${systemSymbol}/waypoints`});
	dbSystems.setSystemWaypoints(systemSymbol, waypoints);
	return waypoints;
}
