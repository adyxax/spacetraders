//import events from 'events';
//
//const bus = new events.EventEmitter();

import { debugLog } from '../lib/api.ts';
import { Ship } from '../lib/ships.ts';
import { market, trait, waypoint } from '../lib/systems.ts';
import { Waypoint } from '../lib/types.ts';
import {
	distance,
	sortByDistanceFrom,
	sortByPrice,
	whatCanBeTradedAt,
} from '../lib/utils.ts';

async function navigate_to_nearest_exporting_waypoint(ship: Ship): Promise<void> {
	const shipWaypoint = await waypoint(ship.nav.waypointSymbol);
	const marketplaceWaypoints = await trait(ship.nav.systemSymbol, 'MARKETPLACE');
	const candidates: Array<Waypoint> = [];
	for (const w of marketplaceWaypoints) {
		const marketplaceData = await market(w);
		if (!marketplaceData.exports) continue;
		candidates.push(w);
	}
	const next = sortByDistanceFrom(shipWaypoint, candidates)[0].data;
	await ship.navigate(next);
}

export async function run(ship: Ship): Promise<void> {
	while (true) {
		const shipWaypoint = await waypoint(ship.nav.waypointSymbol);
		const marketplaceData = await market(shipWaypoint);
		if (marketplaceData.exports.length === 0) {
			await navigate_to_nearest_exporting_waypoint(ship);
			continue;
		}
		const marketplaceWaypoints = await trait(ship.nav.systemSymbol, 'MARKETPLACE');
		const candidates: Array<{price: number, tradeVolume: number, symbol: string, waypoint: Waypoint}> = [];
		for (const w of marketplaceWaypoints) {
			const data = await market(w);
			const trades = marketplaceData.exports.filter(g => data.imports.some(h => g.symbol === h.symbol));
			if (!trades) continue;
			const exports = marketplaceData.tradeGoods.filter(g => trades.some(h => g.symbol === h.symbol));
			const imports = data.tradeGoods.filter(g => trades.some(h => g.symbol === h.symbol));
			for (const e of exports) {
				const i = imports.filter(g => g.symbol === e.symbol)[0];
				const price = i.sellPrice - e.purchasePrice - distance(w, shipWaypoint);
				candidates.push({price: price, tradeVolume: e.tradeVolume, symbol: e.symbol, waypoint: w});
				debugLog({distance: distance(w, shipWaypoint), purchasePrice: e.purchasePrice, sellPrice: i.sellPrice, tradeVolume: e.tradeVolume, symbol: e.symbol, waypoint: w});
			}
		}
		sortByPrice(candidates);
		debugLog(candidates[0]);
		throw "STOP";
		while(!ship.isFull()) {
			await ship.purchase(candidates[0].symbol, Math.min(candidates[0].tradeVolume, ship.cargo.capacity - ship.cargo.units));
		}
		await ship.navigate(candidates[0].waypoint);
		while (!ship.isEmpty()) {
			await ship.sell(candidates[0].symbol, Math.min(candidates[0].tradeVolume, ship.cargo.inventory.filter(i => i.symbol === candidates[0].symbol)[0].units));
		}
	}
}
