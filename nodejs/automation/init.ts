import * as db from '../database/db.ts';
import * as dbTokens from '../database/tokens.ts';
import {
	Response,
} from '../lib/api.ts';
import { Agent, initAgent, setAgent } from '../lib/agent.ts';
import { Contract } from '../lib/contracts.ts';
import { Ship } from '../lib/ships.ts';
import * as libContracts from '../lib/contracts.ts';

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
				await initAgent();
				return;
			default:
				throw json;
		}
	}
	db.reset();
	dbTokens.addToken(json.data.token);
	setAgent(json.data.agent);
}
