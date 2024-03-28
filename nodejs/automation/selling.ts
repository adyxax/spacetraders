import * as dbMarkets from '../database/markets.ts';
import * as dbShips from '../database/ships.ts';
import * as api from '../lib/api.ts';
import * as libShips from '../lib/ships.ts';
import * as libSystems from '../lib/systems.ts';
import * as utils from '../lib/utils.ts';
import { CargoManifest } from '../model/cargo.ts';
import { CommonThing } from '../model/common.ts';
import { Ship } from '../model/ship.ts';

// example ctx { ship: {XXX}, keep: 'SILVER_ORE' }
export async function sell(ship: Ship, good: string): Promise<Ship> {
    outer: while(true) {
		ship = dbShips.getShip(ship.symbol);
		// first lets see what we want to sell
		let cargo = utils.categorizeCargo(ship.cargo, good);
		// get the marketdata from our location
		const market = await libSystems.market(ship.nav.waypointSymbol);
		// can we sell anything here?
		const goods = whatCanBeTradedAt(cargo.goods, market.imports.concat(market.exchange));
		for (let i = 0; i < goods.length; i++) {
			await libShips.sell(ship, goods[i].symbol);
		};
		// are we done selling everything we can?
		ship = dbShips.getShip(ship.symbol);
		cargo = utils.categorizeCargo(ship.cargo, good);
		if (Object.keys(cargo.goods).length === 0) {
			return ship;
		}
		// we need to move somewhere else to sell our remaining goods
		// first we look into markets in our system
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
		// check from the closest one if they import what we need to sell
		for (let i = 0; i < markets.length; i++) {
			const waypointSymbol = markets[i].data.symbol;
			const market = await libSystems.market(waypointSymbol);
			// if we have no data on the market we need to go there and see
			// and if we have data and can sell there we need to go too
			if (market === null || whatCanBeTradedAt(cargo.goods, market.imports).length > 0) {
				await libShips.navigate(ship, waypointSymbol);
				continue outer;
			}
		}
		// check from the closest one if they exchange what we need to sell
		for (let i = 0; i < markets.length; i++) {
			const waypointSymbol = markets[i].data.symbol;
			const market = await libSystems.market(waypointSymbol);
			// if we can sell there we need to go
			if (whatCanBeTradedAt(cargo.goods, market.exchange).length > 0) {
				await libShips.navigate(ship, waypointSymbol);
				continue outer;
			}
		}
		throw new Error(`Ship {ship.symbol} has found no importing or exchanging market for its cargo in the system`);
    }
}

function whatCanBeTradedAt(cargo: CargoManifest, goods: Array<CommonThing>): Array<CommonThing> {
    return goods.filter(g => cargo[g.symbol] !== undefined );
}
