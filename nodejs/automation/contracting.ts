import { Contract } from '../model/contract.ts';
import { Ship } from '../model/ship.ts';
import * as mining from './mining.js';
import * as selling from './selling.js';
import * as dbContracts from '../database/contracts.ts';
import * as dbShips from '../database/ships.ts';
import * as api from '../lib/api.ts';
import * as contracts from '../lib/contracts.ts';
import * as libShips from '../lib/ships.ts';
import * as libSystems from '../lib/systems.ts';
import * as systems from '../lib/systems.ts';
import * as utils from '../lib/utils.ts';

export async function init(): Promise<void> {
	const ship = dbShips.getShips()[0]; // This should always be the command ship
	while(true) { // we use the fact that there can only be at most one active contract at a time
		const contracts = dbContracts.getContracts().filter(c => !c.fulfilled);
		let contract: Contract;
		if (contracts.length === 0) {
			contract = await libShips.negotiate(ship);
		} else {
			contract = contracts[0];
		}
		await run(contract, ship);
		await libShips.negotiate(ship);
	}
}

async function run(contract: Contract, ship: Ship): Promise<void> {
	await contracts.accept(contract);
	switch(contract.type) {
		case 'PROCUREMENT':
			if (contract.terms.deliver[0].tradeSymbol.match(/_ORE$/)) {
				await runOreProcurement(contract, ship);
			} else {
				await runTradeProcurement(contract, ship);
			}
			break;
		default:
			throw `Handling of contract type ${contract.type} is not implemented yet`;
	}
}

async function runOreProcurement(contract: Contract, ship: Ship): Promise<void> {
	const wantedCargo = contract.terms.deliver[0].tradeSymbol;
	const deliveryPoint = contract.terms.deliver[0].destinationSymbol;
	const asteroids = await systems.type(ship.nav.systemSymbol, 'ENGINEERED_ASTEROID');
	const asteroidSymbol = asteroids[0].symbol;
	while (!contract.fulfilled) {
		ship = dbShips.getShip(ship.symbol);
		const goodCargo = ship.cargo.inventory.filter(i => i.symbol === wantedCargo)[0]
		// what we do depends on where we are
		switch (ship.nav.waypointSymbol) {
			case asteroidSymbol:
				await mining.mineUntilFullFor(contract, ship, asteroidSymbol);
				await libShips.navigate(ship, deliveryPoint);
				break;
			case deliveryPoint:
				if (goodCargo !== undefined) { // we could be here if a client restart happens right after selling before we navigate away
					contract = await contracts.deliver(contract, ship);
					if (contract.fulfilled) return;
				}
				await libShips.navigate(ship, asteroidSymbol);
				break;
			default:
				await selling.sell(ship, wantedCargo);
				await libShips.navigate(ship, asteroidSymbol);
		}
	}
}

async function runTradeProcurement(contract: Contract, ship: Ship): Promise<void> {
	const deliver = contract.terms.deliver[0];
	const deliveryPoint = deliver.destinationSymbol;
	const wantedCargo = deliver.tradeSymbol;
	while (!contract.fulfilled) {
		ship = dbShips.getShip(ship.symbol);
		const goodCargo = ship.cargo.inventory.filter(i => i.symbol === wantedCargo)[0]
		// make sure we are not carrying useless stuff
		await selling.sell(ship, wantedCargo);
		// go buy what we need
		const rawMarkets = await libSystems.trait(ship.nav.systemSymbol, 'MARKETPLACE');
		// sorted by distance from where we are
		const markets = rawMarkets.map(function (m) { return {
			data: m,
			distance: (m.x - ship.nav.route.destination.x) ** 2 + (m.y - ship.nav.route.destination.y) ** 2,
		}});
		markets.sort(function(a, b) {
			if (a.distance < b.distance) {
				return -1;
			} else if (a.distance > b.distance) {
				return 1;
			}
			return 0;
		});
		// check from the closest one that exports what we need
		let buyingPoint: string = null;
		outer: for (let i = 0; i < markets.length; i++) {
			const waypointSymbol = markets[i].data.symbol;
			const market = await libSystems.market(waypointSymbol);
			for (let j = 0; j < market.exports; j++) {
				if (market.exports[j].symbol === wantedCargo) {
					buyingPoint = market.symbol;
					break outer;
				}
			}
		}
		// if we did not find an exporting market we look for an exchange
		if (buyingPoint === null) {
			outer: for (let i = 0; i < markets.length; i++) {
				const waypointSymbol = markets[i].data.symbol;
				const market = await libSystems.market(waypointSymbol);
				for (let j = 0; j < market.exchanges; j++) {
					if (market.exports[j].symbol === wantedCargo) {
						buyingPoint = market.symbol;
						break outer;
					}
				}
			}
		}
		if (buyingPoint === null) {
			throw `runTradeProcurement failed, no market exports or exchanges ${wantedCargo}`;
		}
		// go buy what we need
		await libShips.navigate(ship, buyingPoint);
		const units = Math.min(
			deliver.unitsRequired - deliver.unitsFulfilled,
			ship.cargo.capacity - ship.cargo.units,
		);
		await libShips.buy(ship, wantedCargo, units);
		// then make a delivery
		await libShips.navigate(ship, deliveryPoint);
		contract = await contracts.deliver(contract, ship);
		if (contract.fulfilled) return;
	}
	console.log("runTradeProcurement not implemented");
	throw "not implemented";
}
