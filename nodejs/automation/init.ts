import * as dbAgents from '../database/agents.ts';
import * as db from '../database/db.ts';
import * as dbContracts from '../database/contracts.ts';
import * as dbShips from '../database/ships.ts';
import * as dbTokens from '../database/tokens.ts';
import { Agent } from '../model/agent.ts';
import { Response } from '../model/api.ts';
import { Contract } from '../model/contract.ts';
import { Ship } from '../model/ship.ts';
import * as api from '../lib/api.ts';
import * as libContracts from '../lib/contracts.ts';
import * as libShips from '../lib/ships.ts';

const symbol = process.env.NODE_ENV === 'test' ? 'ADYXAX-0' : 'ADYXAX-JS';

// This function registers then inits the database
export async function init(): Promise<void> {
	const response = await fetch('https://api.spacetraders.io/v2/register', {
		method: 'POST',
		headers: {
			'Content-Type': 'application/json',
		},
		body: JSON.stringify({
			symbol: symbol,
			faction: "COSMIC",
		}),
	});
    const json = await response.json() as Response<{agent: Agent, contract: Contract, ship: Ship, token: string}>;
	if (json.error !== undefined) {
		switch(json.error?.code) {
			case 4111:  // 4111 means the agent symbol has already been claimed so no server reset happened
				// TODO await agents.agents();
				const contracts = await libContracts.getContracts();
				const ongoing = contracts.filter(c => !c.fulfilled);
				const ships = await libShips.getShips();
				if (ongoing.length === 0) libShips.negotiate(ships[0]);
				return;
			default:
				throw json;
		}
	}
	db.reset();
	dbTokens.addToken(json.data.token);
	dbAgents.addAgent(json.data.agent);
	dbContracts.setContract(json.data.contract);
	dbShips.setShip(json.data.ship);
	// Temporary fix to fetch the data on the startup probe
	await libShips.getShips();
}
