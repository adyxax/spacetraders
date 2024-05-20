import events from 'events';

import * as autoContracting from './contracting.ts';
import { debugLog, send, sleep } from '../lib/api.ts';
import { getAgent } from '../lib/agent.ts';
import { getShips, Ship } from '../lib/ships.ts';
import { market, shipyard, trait, waypoint } from '../lib/systems.ts';
import { Waypoint } from '../lib/types.ts';
import {
	distance,
	sortByDistanceFrom,
} from '../lib/utils.ts';

const bus = new events.EventEmitter(); // a bus to notify the agent to start purchasing ships
let running = false;
let state = 0;
enum states {
	start_running_contracts_with_the_command_ship = 0,
	visit_all_shipyards,
}

export async function run(): Promise<void> {
	if (running) {
		throw 'refusing to start a second agent processor';
	}
	running = true;
	state = 0;
	try {
		while(true) {
			const ships = getShips();
			switch(state) {
				case states.start_running_contracts_with_the_command_ship:
					//await autoContracting.run(ships[0]);
					state++;
					continue;
				case states.visit_all_shipyards:
					await visit_all_shipyards(ships[1]);
					state++;
					continue;
				default:
					debugLog('No more agent processor states implemented, exiting!')
					return;
			}
		}
	} catch (e) {
		running = false;
		throw e;
	}
}

async function visit_all_shipyards(probe: Ship) {
	const probeWaypoint = await waypoint(probe.nav.waypointSymbol);
	const shipyardWaypoints = await trait(probe.nav.systemSymbol, 'SHIPYARD');
	let candidates: Array<Waypoint> = [];
	for (const w of shipyardWaypoints) {
		const shipyardData = await shipyard(w);
		if (shipyardData.ships) continue;
		candidates.push(w);
	}
	const nexts = sortByDistanceFrom(probeWaypoint, candidates).map(o => o.data);
	for (const next of nexts) {
		await probe.navigate(next);
		await market(next);
		await shipyard(next);
	}
}
