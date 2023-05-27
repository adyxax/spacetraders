import { registerAgent } from '../database/config.js';

// This function inits the database in case we have an already registered game
export function init(symbol, faction, token) {
	registerAgent(symbol, faction, token);
	// TODO ships
	// TODO contract
	// TODO agent
}

// This function registers then inits the database
export function register(symbol, faction) {
	fetch(
		'https://api.spacetraders.io/v2/register',
		{
			method: 'POST',
			headers: {
				'Content-Type': 'application/json',
			},
			body: JSON.stringify({
				symbol: symbol,
				faction: faction,
			}),
		})
		.then(response => response.json())
		.then(response => {
			console.log(JSON.stringify(response, null, 2));
			init(symbol, faction, response.data.token);
			// TODO ship
			// TODO contract
			// TODO agent
		})
		.catch(err => console.error(err));
}
