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
	send_the_starting_probe_to_a_shipyard_that_sells_probes,
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
					// TODO await autoContracting.run(ships[0]);
					state++;
					continue;
				case states.visit_all_shipyards:
					await visit_all_shipyards(ships[1]);
					state++;
					continue;
				case states.send_the_starting_probe_to_a_shipyard_that_sells_probes:
					await send_the_starting_probe_to_a_shipyard_that_sells_probes(ships[1]);
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

async function send_the_starting_probe_to_a_shipyard_that_sells_probes(probe: Ship) {
	const probeWaypoint = await waypoint(probe.nav.waypointSymbol);
	const myShipyard = await shipyard(probeWaypoint);
	if (myShipyard.shipTypes.some(t => t.type === 'SHIP_PROBE')) return;
	// our starting probe is not at a shipyard that sells probes, let's move
	const shipyardWaypoints = await trait(probe.nav.systemSymbol, 'SHIPYARD');
	let candidates: Array<{price: number, waypoint: Waypoint}> = [];
	for (const w of shipyardWaypoints) {
		const shipyardData = await shipyard(w);
		const probeData = shipyardData.ships.filter(t => t.type === 'SHIP_PROBE');
		if (probeData.length === 0) continue;
		candidates.push({price: probeData[0].purchasePrice, waypoint: w });
	};
	candidates.sort(function(a, b) {
		if (a.price < b.price) {
			return -1;
		} else if (a.price > b.price) {
			return 1;
		}
		return 0;
	});
	await probe.navigate(candidates[0].waypoint);
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
