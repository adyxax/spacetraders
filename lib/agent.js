import { registerAgent } from '../database/config.js';
import * as api from './api.js';
import * as contracts from './contracts.js';
import * as ships from './ships.js';
import * as systems from './systems.js';

export async function auto(ctx) {
	let ship = await ships.ship({ship: ctx.ship});
	// Fetch our contracts in the system the ship currently is in
	let cs = await contracts.contracts();
	cs = cs.data.filter(c => c.terms.deliver[0].destinationSymbol.startsWith(ship.data.nav.systemSymbol));
	if (cs === []) throw `No contract at ${ctx.ship}'s location`;
	let contract = cs[0];
	if (!contract.accepted) {
		console.log(new Date(), `accepting contract ${contract.id}`);
		await contracts.accept({id: contract.id});
	}
	const good = contract.terms.deliver[0].tradeSymbol;
	const deliveryPoint = contract.terms.deliver[0].destinationSymbol;
	const asteroidFields = await systems.type({symbol: ship.data.nav.systemSymbol, type: 'ASTEROID_FIELD'});
	const asteroidField = asteroidFields[0].symbol;
	while (true) {
		ship = await ships.ship({ship: ctx.ship}); // TODO we should not need to fetch this
		// If we are in transit, we wait until we arrive
		const delay = new Date(ship.data.nav.route.arrival) - new Date();
		if (delay > 0) await api.sleep(delay);
		// Then it depends on where we are
		let goodCargo = ship.data.cargo.inventory.filter(i => i.symbol === good)[0];
		// the switch makes this 'resumable'
		switch (ship.data.nav.waypointSymbol) {
		case asteroidField:
			let response = await mineUntilFullOf({good: good, ship: ctx.ship});
			//console.log(`${ctx.ship}'s cargo is full with ${response.units} of ${good}!`);
			await ships.navigate({ship: ctx.ship, waypoint: deliveryPoint});
			break;
		case deliveryPoint:
			await ships.dock({ship: ctx.ship});
			await ships.refuel({ship: ctx.ship});
			console.log(`delivering ${goodCargo.units} of ${good}`);
			await contracts.deliver({contract: contract.id, ship: ctx.ship, good: good, units: goodCargo.units });
			await ships.navigate({ship: ctx.ship, waypoint: asteroidField});
			await ships.dock({ship: ctx.ship});
			await ships.refuel({ship: ctx.ship});
			await ships.orbit({ship: ctx.ship});
			break;
		default:
			throw `where is the ship?`;
		}
	}
}

// example ctx { good: 'SILVER_ORE', ship: 'ADYXAX-2' }
// returns the number of units of the good the ship extracted
async function mineUntilFullOf(ctx) {
	while(true) {
		let response = await mineUntilFull({ship: ctx.ship});
		if (response === null) response = await ships.ship({ship: ctx.ship}); // TODO we should not need to fetch this
		let good = response.data.cargo.inventory.filter(i => i.symbol === ctx.good)[0];
		const inventory = response.data.cargo.inventory.filter(i => i.symbol !== ctx.good);
		const antimatter = response.data.cargo.inventory.filter(i => i.symbol === 'ANTIMATTER')[0];
		if (good?.units + (antimatter?.units ?? 0) >= response.data.cargo.capacity * 0.9) { // > 90% full of the valuable goods
			return good.units;
		} else { // we are full but need to sell junk
			await ships.dock({ship: ctx.ship});
			for (let i=0; i<inventory.length; ++i) {
				if (inventory[i].symbol === 'ANTIMATTER') continue;
				//console.log(`selling ${inventory[i].units} of ${inventory[i].symbol}`);
				await ships.sell({ship: ctx.ship, good: inventory[i].symbol, units: inventory[i].units});
			}
			await ships.orbit({ship: ctx.ship});
		}
	}
}

// example ctx { ship: 'ADYXAX-2' }
// returns the last ship's extract api response
async function mineUntilFull(ctx) {
	while(true) {
		const response = await ships.extract(ctx);
		if (response === null) return null;
		//console.log(`${ctx.ship}: extracted ${response.data.extraction.yield.units} of ${response.data.extraction.yield.symbol}`);
		await api.sleep(response.data.cooldown.remainingSeconds*1000);
		if (response.data.cargo.units >= response.data.cargo.capacity * 0.9) return response;
	}
}

// This function inits the database in case we have an already registered game
export function init(symbol, faction, token) {
	registerAgent(symbol, faction, token);
	// TODO ships
	// TODO contract
	// TODO agent
}

// This function registers then inits the database
export function register(symbol, faction) {
	fetch(
		'https://api.spacetraders.io/v2/register',
		{
			method: 'POST',
			headers: {
				'Content-Type': 'application/json',
			},
			body: JSON.stringify({
				symbol: symbol,
				faction: faction,
			}),
		})
		.then(response => response.json())
		.then(response => {
			console.log(JSON.stringify(response, null, 2));
			init(symbol, faction, response.data.token);
			// TODO ship
			// TODO contract
			// TODO agent
		})
		.catch(err => console.error(err));
}
