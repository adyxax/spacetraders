import db from '../database/db.js';
import * as dbSystems from '../database/systems.js';
import * as api from '../lib/api.js';

export async function init() {
	const response = await api.send({endpoint: `/systems`, page: Math.max(1, Math.floor(dbSystems.getSystemsCount()/20)), priority: 100});
	if (response.error !== undefined) {
		throw response;
	}
	db.transaction(() => response.forEach(function(system) { try { dbSystems.addSystem(system); } catch {} }))();
}
