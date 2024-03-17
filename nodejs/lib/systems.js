import * as api from './api.js';
import * as dbMarkets from '../database/markets.js';
import * as dbShips from '../database/ships.js';
import * as dbSystems from '../database/systems.js';
import * as utils from './utils.js';

// Retrieves a marketplace's market data for waypointSymbol
export async function market(waypointSymbol) {
    const data = dbMarkets.getMarketAtWaypoint(waypointSymbol);
    if (data === null) {
	if (dbShips.getShipsAt(waypointSymbol) === null) {
	    return null;
	}
	const systemSymbol = utils.systemFromWaypoint(waypointSymbol);
	let d = await api.send({endpoint: `/systems/${systemSymbol}/waypoints/${waypointSymbol}/market`});
	delete d.data.transactions;
	dbMarkets.setMarket(d.data);
	return d;
    }
    return data;
}

// Retrieves a shipyard's information for ctx.symbol
export async function shipyard(ctx) {
	const systemSymbol = utils.systemFromWaypoint(ctx.symbol);
	return await api.send({endpoint: `/systems/${systemSymbol}/waypoints/${ctx.symbol}/shipyard`});
}

// Retrieves the system's information for ctx.symbol and caches it in the database
export async function system(ctx) {
	let s = dbSystems.getSystem(ctx.symbol);
	if (s === null) {
		const response = await api.send({endpoint: `/systems/${ctx.symbol}`});
		if (response.error !== undefined) {
			switch(response.error.code) {
			case 404:
				throw `Error retrieving info for system ${ctx.symbol}: ${response.error.message}`;
			default: // yet unhandled error
				throw response;
			}
		}
		s = response.data;
		dbSystems.setSystem(s);
	}
	return s;
}

// Retrieves a list of waypoints that have a specific ctx.trait like a SHIPYARD or a MARKETPLACE in the system ctx.symbol
export async function trait(ctx) {
	const w = await waypoints(ctx);
	return w.filter(s => s.traits.some(t => t.symbol === ctx.trait));
}

// Retrieves a list of waypoints that have a specific ctx.type like ASTEROID_FIELD in the system ctx.symbol
export async function type(ctx, response) {
	const w = await waypoints(ctx);
	return w.filter(s => s.type === ctx.type);
}

// Retrieves the system's information for ctx.symbol and caches it in the database
export async function waypoints(ctx) {
	await system(ctx);
	let updated = dbSystems.getSystemUpdated(ctx.symbol);
	// TODO handle uncharted systems
	if (updated === null) {
		let waypoints = [];
		for (let page=1; true; ++page) {
			const response = await api.send({endpoint: `/systems/${ctx.symbol}/waypoints?limit=20&page=${page}`, priority: 98});
			if (response.error !== undefined) {
				switch(response.error.code) {
				case 404:
					throw `Error retrieving waypoints for system ${ctx.symbol}: ${response.error.message}`;
				default: // yet unhandled error
					throw response;
				}
			}
			waypoints = waypoints.concat(response.data);
			if (response.meta.total <= response.meta.limit * page) {
				break;
			}
		}
		dbSystems.setSystemWaypoints(ctx.symbol, waypoints);
		return waypoints;
	}
	return dbSystems.getSystem(ctx.symbol).waypoints;
}
