import * as selling from './selling.js';
import * as dbContracts from '../database/contracts.js';
import * as dbShips from '../database/ships.js';
import * as api from '../lib/api.js';
import * as libShips from '../lib/ships.js';
import * as utils from '../lib/utils.js';
import { Contract } from '../model/contract.ts';
import { Ship } from '../model/ship.ts';

export async function mineUntilFullFor(contract: Contract, ship: Ship, asteroidSymbol: string): Promise<void> {
	// TODO find a good asteroid
	while(true) {
		await mineUntilFull(ship);
		contract = dbContracts.getContract(contract.id);
		const deliver = contract.terms.deliver[0];
		ship = dbShips.getShip(ship.symbol);
		const cargo = utils.categorizeCargo(ship.cargo, deliver.tradeSymbol);
		const wantedUnits = Object.values(cargo.wanted).reduce((acc, e) => acc += e, 0);
		// > 90% full of the valuable goods ? Or just have enough for the contract?
		if (wantedUnits >= ship.cargo.capacity * 0.9
			|| cargo.wanted[deliver.tradeSymbol] >= deliver.unitsRequired - deliver.unitsFulfilled) return;
		// we are full but need to sell junk
		await selling.sell(ship, deliver.tradeSymbol);
		await libShips.navigate(ship, asteroidSymbol);
	}
}

async function mineUntilFull(ship: Ship): Promise<void> {
	while (!libShips.isFull(ship)) {
		await libShips.extract(ship);
	}
}

// TODO surveying the asteroid field
