import * as api from './api.js';
import * as db from '../database/systems.js';

// Retrieves a list of waypoints that have a specific ctx.trait like a SHIPYARD or a MARKETPLACE in the system ctx.symbol
export async function trait(ctx) {
	const s = await system(ctx);
	return s.filter(s => s.traits.some(t => t.symbol === ctx.trait));
}

// Retrieves a list of waypoints that have a specific ctx.type like ASTEROID_FIELD in the system ctx.symbol
export async function type(ctx, response) {
	const s = await system(ctx);
	return s.filter(s => s.type === ctx.type);
}

// Retrieves the system's information for ctx.symbol and cache it in the database
export async function system(ctx) {
	let s = db.getSystem(ctx.symbol);
	if (s === null) {
		const response = await api.send({endpoint: `/systems/${ctx.symbol}/waypoints?limit=20&page=1`});
		if (response.error !== undefined) {
			switch(response.error.code) {
			case 404:
				throw `Error retrieving waypoints for system ${ctx.symbol}: ${response.error.message}`;
			default: // yet unhandled error
				throw response;
			}
		}
		if (response.meta !== undefined && response.meta.total > response.meta.limit) {
			throw `Error retrieving waypoints for system ${ctx.symbol}: Pagination is not implemented yet!`;
		}
		s = response.data;
		db.setSystem(ctx.symbol, s);
	}
	return s;
}

export async function systems(ctx) {
	const response = await api.send({endpoint: `/systems?limit=20&page=1`});
	// TODO pagination
	return response;
}

// Retrieves a shipyard's information for ctx.symbol
export async function shipyard(ctx) {
	const systemSymbol = ctx.symbol.match(/([^-]+-[^-]+)/)[1]; // TODO generalise this extraction
	console.log(systemSymbol);
	return await api.send({endpoint: `/systems/${systemSymbol}/waypoints/${ctx.symbol}/shipyard`});
}
