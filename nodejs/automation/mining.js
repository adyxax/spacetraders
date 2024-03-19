import * as selling from './selling.js';
import * as dbShips from '../database/ships.js';
import * as api from '../lib/api.js';
import * as libShips from '../lib/ships.js';
import * as utils from '../lib/utils.js';

// example ctx { asteroidSymbol: XXXXX, good: 'SILVER_ORE', symbol: 'ADYXAX-2' }
// returns the number of units of the good the ship holds
export async function mineUntilFullOf(ctx) {
	while(true) {
		const ship = dbShips.getShip(ctx.symbol);
		const cargo = utils.categorizeCargo(await mineUntilFull({symbol: ctx.symbol}), ctx.good);
		const wantedUnits = Object.values(cargo.wanted).reduce((acc, e) => acc += e, 0);
		// > 90% full of the valuable goods ?
		if (wantedUnits >= ship.cargo.capacity * 0.9) return;
		// we are full but need to sell junk
		await selling.sell(ship, ctx.good);
		await libShips.navigate({symbol: ship.symbol, waypoint: ctx.asteroidSymbol});
	}
}

// example ctx { symbol: 'ADYXAX-2' }
// extract the ship's cargo contents when more than 80% full then returns the ships cargo object
async function mineUntilFull(ctx) {
	while(true) {
		const ship = dbShips.getShip(ctx.symbol);
		if (ship.cargo.units >= ship.cargo.capacity * 0.9) return ship.cargo;
		if (await libShips.extract({symbol: ctx.symbol}) === null)
			await ship(ctx); // refresh the ships status from the server just in case
	}
}

// TODO surveying the asteroid field
