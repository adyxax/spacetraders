import * as dbAgents from '../database/agents.js';
import * as db from '../database/db.js';
import * as dbContracts from '../database/contracts.js';
import * as dbShips from '../database/ships.js';
import * as dbTokens from '../database/tokens.js';
import * as api from '../lib/api.js';
import * as ships from '../lib/ships.js';

// This function registers then inits the database
export async function init() {
	const response = await fetch('https://api.spacetraders.io/v2/register', {
		method: 'POST',
		headers: {
			'Content-Type': 'application/json',
		},
		body: JSON.stringify({
			symbol: "ADYXAX",
			faction: "COSMIC",
		}),
	});
	const json = await response.json();
	if (json.error !== undefined) {
		switch(json.error?.code) {
		case 4111:  // 4111 means the agent symbol has already been claimed so no server reset happened
			return;
		default:
			throw json;
		}
	}
	db.reset();
	dbAgents.addAgent(json.data.agent);
	dbContracts.setContract(json.data.contract);
	dbShips.setShip(json.data.ship);
	dbTokens.addToken(json.data.token);
	// Temporary fix to fetch the data on the startup probe
	ships.ships();
}
