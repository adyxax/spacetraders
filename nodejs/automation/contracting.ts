import { Contract } from '../model/contract.ts';
import { Ship } from '../model/ship.ts';
import * as mining from './mining.js';
import * as selling from './selling.js';
import * as dbContracts from '../database/contracts.ts';
import * as dbShips from '../database/ships.ts';
import * as api from '../lib/api.ts';
import * as contracts from '../lib/contracts.ts';
import * as libShips from '../lib/ships.ts';
import * as systems from '../lib/systems.ts';
import * as utils from '../lib/utils.ts';

export async function init() {
	const cs = dbContracts.getContracts();
	cs.forEach(contract => run(contract));
}

async function run(contract: Contract) {
	await contracts.accept(contract);
	const contractSystem = utils.systemFromWaypoint(contract.terms.deliver[0].destinationSymbol);
	let ships = dbShips.getShipsAt(contractSystem);
	ships = ships.filter(ship => ship.registration.role !== 'SATELLITE'); // filter out probes

	switch(contract.type) {
		case 'PROCUREMENT':
			await runProcurement(contract, ships);
			break;
		default:
			throw `Handling of contract type ${contract.type} is not implemented yet`;
	}
}

async function runProcurement(contract: Contract, ships: Array<Ship>) {
	// TODO check if contract is fulfilled!
	const wantedCargo = contract.terms.deliver[0].tradeSymbol;
	const deliveryPoint = contract.terms.deliver[0].destinationSymbol;
	const asteroids = await systems.type(ships[0].nav.systemSymbol, 'ENGINEERED_ASTEROID');
	const asteroidSymbol = asteroids[0].symbol;
	ships.forEach(async function(ship) {
		while (!contract.fulfilled) {
			ship = dbShips.getShip(ship.symbol) as Ship;
			let goodCargo = ship.cargo.inventory.filter(i => i.symbol === wantedCargo)[0]
			// If we are in transit, we wait until we arrive
			const delay = new Date(ship.nav.route.arrival).getTime() - new Date().getTime();
			if (delay > 0) await api.sleep(delay);
			// Then it depends on where we are
			switch (ship.nav.waypointSymbol) {
				case asteroidSymbol:
					await mining.mineUntilFullOf(wantedCargo, ship, asteroidSymbol);
					await libShips.navigate(ship, deliveryPoint);
					break;
				case deliveryPoint:
					if (goodCargo !== undefined) { // we could be here if a client restart happens right after selling before we navigate away
						console.log(`delivering ${goodCargo.units} of ${wantedCargo}`);
						contract = await contracts.deliver(contract, ship);
						if (contract.fulfilled) break;
					}
					await libShips.navigate(ship, asteroidSymbol);
					break;
				default:
					if (libShips.isFull(ship)) {
						await selling.sell(ship, wantedCargo);
					} else {
						await libShips.navigate(ship, asteroidSymbol);
					}
			}
		}
		// TODO repurpose the ship
	});
}
