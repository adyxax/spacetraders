import * as api from '../lib/api.js';
import * as ships from '../lib/ships.js';

// example ctx { good: 'SILVER_ORE', ship: 'ADYXAX-2' }
// returns the number of units of the good the ship extracted
export async function mineUntilFullOf(ctx) {
	while(true) {
		let response = await mineUntilFull({ship: ctx.ship});
		if (response === null) response = await ships.ship({ship: ctx.ship}); // TODO we should not need to fetch this
		let good = response.data.cargo.inventory.filter(i => i.symbol === ctx.good)[0];
		const inventory = response.data.cargo.inventory.filter(i => i.symbol !== ctx.good);
		const antimatter = response.data.cargo.inventory.filter(i => i.symbol === 'ANTIMATTER')[0];
		if (good?.units + (antimatter?.units ?? 0) >= response.data.cargo.capacity * 0.9) { // > 90% full of the valuable goods
			return good.units;
		} else { // we are full but need to sell junk
			await ships.dock({ship: ctx.ship});
			for (let i=0; i<inventory.length; ++i) {
				if (inventory[i].symbol === 'ANTIMATTER') continue;
				//console.log(`selling ${inventory[i].units} of ${inventory[i].symbol}`);
				await ships.sell({ship: ctx.ship, good: inventory[i].symbol, units: inventory[i].units});
			}
			await ships.orbit({ship: ctx.ship});
		}
	}
}

// example ctx { ship: 'ADYXAX-2' }
// returns the last ship's extract api response
async function mineUntilFull(ctx) {
	while(true) {
		const response = await ships.extract(ctx);
		if (response === null) return null;
		//console.log(`${ctx.ship}: extracted ${response.data.extraction.yield.units} of ${response.data.extraction.yield.symbol}`);
		await api.sleep(response.data.cooldown.remainingSeconds*1000);
		if (response.data.cargo.units >= response.data.cargo.capacity * 0.9) return response;
	}
}
