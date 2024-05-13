import * as selling from './selling.js';
import { Contract } from '../lib/contracts.js';
import { Ship } from '../lib/ships.js';
import {
	Waypoint,
} from '../lib/types.ts';
import { categorizeCargo } from '../lib/utils.ts';

export async function mineUntilFullFor(contract: Contract, ship: Ship, asteroid: Waypoint): Promise<void> {
	// TODO find a good asteroid
	while(true) {
		await mineUntilFull(ship);
		const deliver = contract.terms.deliver[0];
		const cargo = categorizeCargo(ship.cargo, deliver.tradeSymbol);
		const wantedUnits = Object.values(cargo.wanted).reduce((acc, e) => acc += e, 0);
		// > 90% full of the valuable goods ? Or just have enough for the contract?
		if (wantedUnits >= ship.cargo.capacity * 0.9
			|| cargo.wanted[deliver.tradeSymbol] >= deliver.unitsRequired - deliver.unitsFulfilled) return;
		// we are full but need to sell junk
		await selling.sell(ship, deliver.tradeSymbol);
		await ship.navigate(asteroid);
	}
}

async function mineUntilFull(ship: Ship): Promise<void> {
	while (!ship.isFull()) {
		await ship.extract();
	}
}

// TODO surveying the asteroid field
