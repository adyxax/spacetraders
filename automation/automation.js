import * as dbConfig from '../database/config.js';
import * as dbShips from '../database/ships.js';
import * as exploration from './exploration.js';

// This function registers then inits the database
export async function register(symbol, faction) {
	const response = await fetch('https://api.spacetraders.io/v2/register', {
		method: 'POST',
		headers: {
			'Content-Type': 'application/json',
		},
		body: JSON.stringify({
			symbol: symbol,
			faction: faction,
		}),
	});
	const json = await response.json();
	console.log(JSON.stringify(response, null, 2));
	if (response.error !== undefined) {
		throw response;
	}
	dbConfig.registerAgent(json.data);
	exploration.init();
	// TODO ship
	// TODO contract
}
