import { debugLog } from '../lib/api.ts';
import { Ship } from '../lib/ships.ts';
import * as mining from './mining.js';
import * as selling from './selling.js';
import { Contract, getContracts } from '../lib/contracts.ts';
import * as libSystems from '../lib/systems.ts';
import * as systems from '../lib/systems.ts';
import {
	sortByDistanceFrom,
} from '../lib/utils.ts';

export async function run(ship: Ship): Promise<void> {
	const contracts = await getContracts();
	const active = contracts.filter(function(c) {
		if (c.fulfilled) return false;
		const deadline = new Date(c.terms.deadline).getTime();
		const now = new Date().getTime();
		return deadline > now;
	});
	for (const contract of active) {
		await runOne(contract, ship);
	}
	while(true) {
		await runOne(await ship.negotiate(), ship);
	}
}

async function runOne(contract: Contract, ship: Ship): Promise<void> {
	debugLog(contract);
	await contract.accept();
	switch(contract.type) {
		case 'PROCUREMENT':
			//if (contract.terms.deliver[0].tradeSymbol.match(/_ORE$/)) {
			//	await runOreProcurement(contract, ship);
			//} else {
				await runTradeProcurement(contract, ship);
			//}
			break;
		default:
			throw `Handling of contract type ${contract.type} is not implemented yet`;
	}
}

async function runOreProcurement(contract: Contract, ship: Ship): Promise<void> {
	const wantedCargo = contract.terms.deliver[0].tradeSymbol;
	const deliveryPoint = await libSystems.waypoint(contract.terms.deliver[0].destinationSymbol);
	const asteroids = await systems.type(ship.nav.systemSymbol, 'ENGINEERED_ASTEROID');
	const asteroid = await systems.waypoint(asteroids[0].symbol);
	while (!contract.fulfilled) {
		const goodCargo = ship.cargo.inventory.filter(i => i.symbol === wantedCargo)[0]
		// what we do depends on where we are
		switch (ship.nav.waypointSymbol) {
			case asteroid.symbol:
				await mining.mineUntilFullFor(contract, ship, asteroid);
				await ship.navigate(deliveryPoint);
				break;
			case deliveryPoint.symbol:
				if (goodCargo !== undefined) { // we could be here if a client restart happens right after selling before we navigate away
					await contract.deliver(ship);
					if (contract.fulfilled) return;
				}
				await ship.navigate(asteroid);
				break;
			default:
				await selling.sell(ship, wantedCargo);
				await ship.navigate(asteroid);
		}
	}
}

async function runTradeProcurement(contract: Contract, ship: Ship): Promise<void> {
	const deliver = contract.terms.deliver[0];
	const deliveryPoint = await libSystems.waypoint(deliver.destinationSymbol);
	const wantedCargo = deliver.tradeSymbol;
	while (!contract.fulfilled) {
		const goodCargo = ship.cargo.inventory.filter(i => i.symbol === wantedCargo)[0]
		// make sure we are not carrying useless stuff
		await selling.sell(ship, wantedCargo);
		if (ship.cargo.units < ship.cargo.capacity) {
			// go buy what we need
			const markets = sortByDistanceFrom(ship.nav.route.destination, await libSystems.trait(ship.nav.systemSymbol, 'MARKETPLACE'));
			// check from the closest one that exports what we need
			let buyingPoint: string = "";
			outer: for (let i = 0; i < markets.length; i++) {
				const waypoint = await libSystems.waypoint(markets[i].data.symbol);
				const market = await libSystems.market(waypoint);
				for (let j = 0; j < market.exports.length; j++) {
					if (market.exports[j].symbol === wantedCargo) {
						buyingPoint = market.symbol;
						break outer;
					}
				}
			}
			// if we did not find an exporting market we look for an exchange
			if (buyingPoint === "") {
				outer: for (let i = 0; i < markets.length; i++) {
					const waypoint = await libSystems.waypoint(markets[i].data.symbol);
					const market = await libSystems.market(waypoint);
					for (let j = 0; j < market.exchange.length; j++) {
						if (market.exchange[j].symbol === wantedCargo) {
							buyingPoint = market.symbol;
							break outer;
						}
					}
				}
			}
			if (buyingPoint === "") {
				throw `runTradeProcurement failed, no market exports or exchanges ${wantedCargo}`;
			}
			// go buy what we need
			await ship.navigate(await libSystems.waypoint(buyingPoint));
			const units = Math.min(
				deliver.unitsRequired - deliver.unitsFulfilled,
				ship.cargo.capacity - ship.cargo.units,
			);
			await ship.purchase(wantedCargo, units);
		}
		// then make a delivery
		await ship.navigate(deliveryPoint);
		await contract.deliver(ship);
		if (contract.fulfilled) return;
	}
	console.log("runTradeProcurement not implemented");
	throw "not implemented";
}
