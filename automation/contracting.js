import * as mining from './mining.js';
import * as dbShips from '../database/ships.js';
import * as api from '../lib/api.js';
import * as contracts from '../lib/contracts.js';
import * as ships from '../lib/ships.js';
import * as systems from '../lib/systems.js';

export async function auto(ctx) {
	let ship = dbShips.getShip(ctx.symbol);
	// Fetch our contracts in the system the ship currently is in
	let cs = await contracts.contracts();
	cs = cs.data.filter(c => c.terms.deliver[0].destinationSymbol.startsWith(ship.nav.systemSymbol));
	if (cs === []) throw `No contract at ${ctx.symbol}'s location`;
	let contract = cs[0];
	if (!contract.accepted) {
		console.log(new Date(), `accepting contract ${contract.id}`);
		await contracts.accept({contract: contract.id});
	}
	const good = contract.terms.deliver[0].tradeSymbol;
	const deliveryPoint = contract.terms.deliver[0].destinationSymbol;
	const asteroidFields = await systems.type({symbol: ship.nav.systemSymbol, type: 'ASTEROID_FIELD'});
	const asteroidField = asteroidFields[0].symbol;
	while (true) {
		ship = dbShips.getShip(ctx.symbol);
		// If we are in transit, we wait until we arrive
		const delay = new Date(ship.nav.route.arrival) - new Date();
		if (delay > 0) await api.sleep(delay);
		// Then it depends on where we are
		let goodCargo = ship.cargo.inventory.filter(i => i.symbol === good)[0];
		// the switch makes this 'resumable'
		switch (ship.nav.waypointSymbol) {
		case asteroidField:
			let response = await mining.mineUntilFullOf({good: good, symbol: ctx.symbol});
			await ships.navigate({symbol: ctx.symbol, waypoint: deliveryPoint});
			break;
		case deliveryPoint:
			await ships.dock({symbol: ctx.symbol});
			await ships.refuel({symbol: ctx.symbol});
			if (goodCargo !== undefined) {
				console.log(`delivering ${goodCargo.units} of ${good}`);
				await contracts.deliver({contract: contract.id, symbol: ctx.symbol, good: good, units: goodCargo.units });
			}
			await ships.navigate({symbol: ctx.symbol, waypoint: asteroidField});
			await ships.dock({symbol: ctx.symbol});
			await ships.refuel({symbol: ctx.symbol});
			await ships.orbit({symbol: ctx.symbol});
			break;
		default:
			await ships.navigate({symbol: ctx.symbol, waypoint: asteroidField});
			await ships.dock({symbol: ctx.symbol});
			await ships.refuel({symbol: ctx.symbol});
			await ships.orbit({symbol: ctx.symbol});
		}
	}
}
