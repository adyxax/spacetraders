import * as selling from './selling.js';
import * as dbShips from '../database/ships.js';
import * as api from '../lib/api.js';
import * as libShips from '../lib/ships.js';
import * as utils from '../lib/utils.js';
import { Ship } from '../model/ship.ts';

export async function mineUntilFullOf(good: string, ship: Ship, asteroidSymbol: string): Promise<void> {
	// TODO find a good asteroid
	while(true) {
		await mineUntilFull(ship);
		ship = dbShips.getShip(ship.symbol) as Ship;
		const cargo = utils.categorizeCargo(ship.cargo, good);
		const wantedUnits = Object.values(cargo.wanted).reduce((acc, e) => acc += e, 0);
		// > 90% full of the valuable goods ?
		if (wantedUnits >= ship.cargo.capacity * 0.9) return ship;
		// we are full but need to sell junk
		await selling.sell(ship, good);
		await libShips.navigate(ship, asteroidSymbol);
	}
}

// example ctx { symbol: 'ADYXAX-2' }
// extract the ship's cargo contents when more than 80% full then returns the ships cargo object
async function mineUntilFull(ship: Ship): Promise<void> {
	ship = dbShips.getShip(ship.symbol) as Ship;
	while (ship.cargo.units <= ship.cargo.capacity * 0.9) {
		ship.cargo = await libShips.extract(ship);
	}
}

// TODO surveying the asteroid field
