import { registerAgent } from '../database/config.js';
import * as api from './api.js';
import * as ships from './ships.js';

// This starts an extraction loop with a ship which ends when the ship's cargo is at least 90% full with only one desired good
// ctx must must have two attributes: `ship` and `good`
export function extract(ctx, response) {
	if (response !== undefined) {
		if (response.error !== undefined) {
			switch(response.error.code) {
			case 4000: // ship is on cooldown
				setTimeout(extract, response.error.data.cooldown.remainingSeconds  * 1000, ctx);
				return;
			case 4228: // ship is full. Running the ship inventory function to list the cargo so that know if we need to sell
				ships.ship({ship: ctx.ship, next:{action: extract, ship: ctx.ship, good: ctx.good}});
				return;
			default:
				throw response;
			}
		}
		if (response.data.extraction !== undefined && response.data.extraction.yield !== undefined) { // yield won't be defined if we reached this point from an inventory request
			console.log(`${ctx.ship}: extracted ${response.data.extraction.yield.units} of ${response.data.extraction.yield.symbol}`);
		}
		if (response.data.cargo !== undefined && response.data.cargo.capacity * 0.9 <= response.data.cargo.units) { // > 90% full
			const good = response.data.cargo.inventory.filter(i => i.symbol === ctx.good)[0];
			const inventory = response.data.cargo.inventory.filter(i => i.symbol !== ctx.good);
			if (good?.units >= response.data.cargo.capacity * 0.9) { // > 90% full
				console.log(`ship's cargo is full with ${response.data.cargo.units} of ${ctx.good}!`);
				return;
			}
			let actions = [{ action: ships.dock, ship: ctx.ship }];
			inventory.forEach(i => actions.push({action: ships.sell, ship: ctx.ship, good: i.symbol, units: i.units}));
			actions.push({action: ships.orbit, ship: ctx.ship});
			actions.push({action: extract, ship: ctx.ship, good: ctx.good});
			api.chain(actions);
			return;
		} else { // we need to mine more
			if (response.data.cooldown) { // we are on cooldown, call ourselves again in a moment
				setTimeout(extract, response.data.cooldown.remainingSeconds  * 1000, ctx);
				return;
			}
		}
	}
	ships.extract({ship: ctx.ship, good: ctx.good, next: { action: extract, ship: ctx.ship, good: ctx.good }});
}

// This function inits the database in case we have an already registered game
export function init(symbol, faction, token) {
	registerAgent(symbol, faction, token);
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
		})
		.catch(err => console.error(err));
}
