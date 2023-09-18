import * as mining from './mining.js';
import * as dbContracts from '../database/contracts.js';
import * as dbShips from '../database/ships.js';
import * as api from '../lib/api.js';
import * as contracts from '../lib/contracts.js';
import * as libShips from '../lib/ships.js';
import * as systems from '../lib/systems.js';
import * as utils from './utils.js';

export async function init() {
	const cs = dbContracts.getContracts();
	cs.forEach(contract => run(contract));
}

async function run(contract) {
	await contracts.accept({id: contract.id});
	const contractSystem = utils.waypointToSystem(contract.terms.deliver[0].destinationSymbol);
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

async function runProcurement(contract, ships) {
	// TODO check if contract is fulfilled!
	const wantedCargo = contract.terms.deliver[0].tradeSymbol;
	const deliveryPoint = contract.terms.deliver[0].destinationSymbol;
	const asteroidFields = await systems.type({symbol: ships[0].nav.systemSymbol, type: 'ASTEROID_FIELD'});
	const asteroidField = asteroidFields[0].symbol;
	ships.forEach(async function(ship) {
		while (!contract.fulfilled) {
			ship = dbShips.getShip(ship.symbol);
			let goodCargo = ship.cargo.inventory.filter(i => i.symbol === wantedCargo)[0];
			// If we are in transit, we wait until we arrive
			const delay = new Date(ship.nav.route.arrival) - new Date();
			if (delay > 0) await api.sleep(delay);
			// Then it depends on where we are
			switch (ship.nav.waypointSymbol) {
			case asteroidField:
				let response = await mining.mineUntilFullOf({good: wantedCargo, symbol: ship.symbol});
				await libShips.navigate({symbol: ship.symbol, waypoint: deliveryPoint});
				break;
			case deliveryPoint:
				if (goodCargo !== undefined) { // we could be here if a client restart happens right after selling before we navigate away
					console.log(`delivering ${goodCargo.units} of ${wantedCargo}`);
					await contracts.deliver({contract: contract.id, symbol: ship.symbol, good: wantedCargo, units: goodCargo.units });
					// TODO check if contract is fulfilled!
				}
				await libShips.navigate({symbol: ship.symbol, waypoint: asteroidField});
				break;
			default:
				await libShips.navigate({symbol: ship.symbol, waypoint: asteroidField});
			}
		}
	});
}
