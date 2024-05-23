import * as autoContracting from './contracting.ts';
import { debugLog, send, sleep } from '../lib/api.ts';
import { getAgent } from '../lib/agent.ts';
import { getShips, purchaseShip, Ship } from '../lib/ships.ts';
import { market, shipyard, trait, waypoint } from '../lib/systems.ts';
import { Waypoint } from '../lib/types.ts';
import {
	are_we_done_visiting_all_markets,
	distance,
	is_there_a_ship_at_this_waypoint,
	sortByDistanceFrom,
} from '../lib/utils.ts';

let running = false;
let state = 0;
enum states {
	start_running_contracts_with_the_command_ship = 0,
	visit_all_shipyards,
	visit_all_markets,
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
					autoContracting.run(ships[0]);
					state++;
					continue;
				case states.visit_all_shipyards:
					await visit_all_shipyards(ships[1]);
					state++;
					continue;
				case states.visit_all_markets:
					await visit_all_markets();
					state++;
					continue;
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

async function visit_all_markets(): Promise<void> {
	if (await are_we_done_visiting_all_markets()) return;
	// send all our probes except the starting one to map the system's markets
	for (const probe of getShips().slice(2)) {
		if (probe.registration.role !== 'SATELLITE') continue;
		visit_next_markets(probe);
		await sleep(10000); // we do not send them all at once so they can chose different destinations
	}
	// buy more probes to speed up the process
	while (true) {
		if (await are_we_done_visiting_all_markets()) return;
		while (getAgent().credits < 250000) {
			await sleep(60000);
		}
		const probe = await purchaseShip('SHIP_PROBE');
		visit_next_markets(probe);
		await sleep(10000);
	}
}

async function visit_next_markets(probe: Ship): Promise<void> {
	while (true) {
		const probeWaypoint = await waypoint(probe.nav.waypointSymbol);
		const marketplaceWaypoints = await trait(probe.nav.systemSymbol, 'MARKETPLACE');
		let candidates: Array<Waypoint> = [];
		for (const w of marketplaceWaypoints) {
			const marketplaceData = await market(w);
			if (marketplaceData.tradeGoods) continue;
			if (is_there_a_ship_at_this_waypoint(w)) continue;
			candidates.push(w);
		}
		if (candidates.length === 0) {
			return;
		}
		const next = sortByDistanceFrom(probeWaypoint, candidates)[0].data;
		await probe.navigate(next);
		await market(next);
	}
}

async function visit_all_shipyards(probe: Ship): Promise<void> {
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
