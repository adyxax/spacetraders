import * as selling from './selling.js';
import * as dbContracts from '../database/contracts.js';
import { Ship } from '../lib/ships.js';
import { Contract } from '../lib/types.ts';
import { categorizeCargo } from '../lib/utils.ts';

export async function mineUntilFullFor(contract: Contract, ship: Ship, asteroidSymbol: string): Promise<void> {
	// TODO find a good asteroid
	while(true) {
		await mineUntilFull(ship);
		contract = dbContracts.getContract(contract.id);
		const deliver = contract.terms.deliver[0];
		const cargo = categorizeCargo(ship.cargo, deliver.tradeSymbol);
		const wantedUnits = Object.values(cargo.wanted).reduce((acc, e) => acc += e, 0);
		// > 90% full of the valuable goods ? Or just have enough for the contract?
		if (wantedUnits >= ship.cargo.capacity * 0.9
			|| cargo.wanted[deliver.tradeSymbol] >= deliver.unitsRequired - deliver.unitsFulfilled) return;
		// we are full but need to sell junk
		await selling.sell(ship, deliver.tradeSymbol);
		await ship.navigate(asteroidSymbol);
	}
}

async function mineUntilFull(ship: Ship): Promise<void> {
	while (!ship.isFull()) {
		await ship.extract();
	}
}

// TODO surveying the asteroid field
