import * as dbShips from '../database/ships.js';
import * as api from '../lib/api.js';
import * as ships from '../lib/ships.js';

// example ctx { good: 'SILVER_ORE', symbol: 'ADYXAX-2' }
// returns the number of units of the good the ship holds
export async function mineUntilFullOf(ctx) {
	while(true) {
		let cargo = await mineUntilFull({symbol: ctx.symbol});
		let good = cargo.inventory.filter(i => i.symbol === ctx.good)[0];
		const antimatter = cargo.inventory.filter(i => i.symbol === 'ANTIMATTER')[0];
		const junk = cargo.inventory.filter(i => i.symbol !== ctx.good && i.symbol !== 'ANTIMATTER');
		if ((good?.units ?? 0) + (antimatter?.units ?? 0) >= cargo.capacity * 0.9) { // > 90% full of the valuable goods
			return good.units;
		} else { // we are full but need to sell junk
			for (let i=0; i<junk.length; ++i) {
				await ships.sell({symbol: ctx.symbol, good: junk[i].symbol, units: junk[i].units});
			}
		}
	}
}

// example ctx { symbol: 'ADYXAX-2' }
// extract the ship's cargo contents when more than 80% full then returns the ships cargo object
async function mineUntilFull(ctx) {
	while(true) {
		const ship = dbShips.getShip(ctx.symbol);
		if (ship.cargo.units >= ship.cargo.capacity * 0.9) return ship.cargo;
		if (await ships.extract({symbol: ctx.symbol}) === null)
			ship = await ship(ctx); // refresh the ships status from the server just in case
	}
}

// TODO surveying the asteroid field
