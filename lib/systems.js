import * as api from './api.js';
import * as db from '../database/systems.js';

// Retrieves all systems information, should be called only once after registering
export async function init(ctx) {
	if (db.isInit()) {
		return;
	}
	for (let page=1; true; ++page) {
		const response = await api.send({endpoint: `/systems?limit=20&page=${page}`, priority:100});
		if (response.error !== undefined) {
			throw response;
		}
		response.data.forEach(system => db.setSystem(system));
		if (response.meta.total <= response.meta.limit * page) {
			break;
		}
	}
	console.log('Finished retrieving all systems information');
	db.init();
}

// Retrieves a shipyard's information for ctx.symbol
export async function shipyard(ctx) {
	const systemSymbol = ctx.symbol.match(/([^-]+-[^-]+)/)[1]; // TODO generalise this extraction
	console.log(systemSymbol);
	return await api.send({endpoint: `/systems/${systemSymbol}/waypoints/${ctx.symbol}/shipyard`});
}

// Retrieves the system's information for ctx.symbol and caches it in the database
export async function system(ctx) {
	let s = db.getSystem(ctx.symbol);
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
		db.setSystem(s);
	}
	return s;
}

// Retrieves a list of waypoints that have a specific ctx.trait like a SHIPYARD or a MARKETPLACE in the system ctx.symbol
export async function trait(ctx) {
	const s = await system(ctx);
	return s.waypoints.filter(s => s.traits.some(t => t.symbol === ctx.trait));
}

// Retrieves a list of waypoints that have a specific ctx.type like ASTEROID_FIELD in the system ctx.symbol
export async function type(ctx, response) {
	const s = await system(ctx);
	return s.waypoints.filter(s => s.type === ctx.type);
}

// Retrieves the system's information for ctx.symbol and caches it in the database
export async function waypoints(ctx) {
	await system(ctx);
	let updated = db.getSystemUpdated(ctx.symbol);
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
		db.setSystemWaypoints(ctx.symbol, waypoints);
		return waypoints;
	}
	return db.getSystem(ctx.symbol).waypoints;
}
