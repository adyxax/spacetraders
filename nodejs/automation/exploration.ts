import db from '../database/db.ts';
import * as dbSystems from '../database/systems.ts';
import { System } from '../lib/types.ts';
import * as api from '../lib/api.ts';

export async function init(): Promise<void> {
	const response = await api.sendPaginated<System>({endpoint: `/systems`, page: Math.max(1, Math.floor(dbSystems.getSystemsCount()/20)), priority: 100});
	db.transaction(() => response.forEach(function(system) { try { dbSystems.addSystem(system); } catch {} }))();
}
