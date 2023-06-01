import * as mining from './mining.js';
import * as api from '../lib/api.js';
import * as contracts from '../lib/contracts.js';
import * as ships from '../lib/ships.js';
import * as systems from '../lib/systems.js';

export async function auto(ctx) {
	let ship = await ships.ship({symbol: ctx.ship});
	// Fetch our contracts in the system the ship currently is in
	let cs = await contracts.contracts();
	cs = cs.data.filter(c => c.terms.deliver[0].destinationSymbol.startsWith(ship.data.nav.systemSymbol));
	if (cs === []) throw `No contract at ${ctx.ship}'s location`;
	let contract = cs[0];
	if (!contract.accepted) {
		console.log(new Date(), `accepting contract ${contract.id}`);
		await contracts.accept({contract: contract.id});
	}
	const good = contract.terms.deliver[0].tradeSymbol;
	const deliveryPoint = contract.terms.deliver[0].destinationSymbol;
	const asteroidFields = await systems.type({symbol: ship.data.nav.systemSymbol, type: 'ASTEROID_FIELD'});
	const asteroidField = asteroidFields[0].symbol;
	while (true) {
		ship = await ships.ship({symbol: ctx.ship}); // TODO we should not need to fetch this
		// If we are in transit, we wait until we arrive
		const delay = new Date(ship.data.nav.route.arrival) - new Date();
		if (delay > 0) await api.sleep(delay);
		// Then it depends on where we are
		let goodCargo = ship.data.cargo.inventory.filter(i => i.symbol === good)[0];
		// the switch makes this 'resumable'
		switch (ship.data.nav.waypointSymbol) {
		case asteroidField:
			let response = await mining.mineUntilFullOf({good: good, ship: ctx.ship});
			//console.log(`${ctx.ship}'s cargo is full with ${response.units} of ${good}!`);
			await ships.navigate({symbol: ctx.ship, waypoint: deliveryPoint});
			break;
		case deliveryPoint:
			await ships.dock({symbol: ctx.ship});
			await ships.refuel({symbol: ctx.ship});
			console.log(`delivering ${goodCargo.units} of ${good}`);
			await contracts.deliver({contract: contract.id, ship: ctx.ship, good: good, units: goodCargo.units });
			await ships.navigate({symbol: ctx.ship, waypoint: asteroidField});
			await ships.dock({symbol: ctx.ship});
			await ships.refuel({symbol: ctx.ship});
			await ships.orbit({symbol: ctx.ship});
			break;
		default:
			throw `where is the ship?`;
		}
	}
}
