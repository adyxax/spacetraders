import {
	debugLog,
} from './api.ts';
import { PriorityQueue } from './priority_queue.ts';
import { getShips } from './ships.ts';
import { market } from './systems.ts';
import {
	Cargo,
	CargoManifest,
	CommonThing,
	Waypoint,
} from './types.ts';

export type CategorizedCargo = {
	wanted: CargoManifest;
	goods: CargoManifest;
};

type Point = {
	x: number;
	y: number;
};

// cargo is a ship.cargo object, want is an optional symbol
export function categorizeCargo(cargo: Cargo, want?: string): CategorizedCargo {
	const wanted = cargo.inventory.filter(i => i.symbol === want || i.symbol === 'ANTIMATTER');
	const goods = cargo.inventory.filter(i => i.symbol !== want && i.symbol !== 'ANTIMATTER');
	const wobj = wanted.reduce(function(acc: CargoManifest, e) {
		acc[e.symbol] = e.units;
		return acc;
	}, {});
	const gobj = goods.reduce(function(acc: CargoManifest, e) {
		acc[e.symbol] = e.units;
		return acc;
	}, {});
	return {wanted: wobj, goods: gobj};
}

export function distance(a: Point, b: Point) {
	return Math.sqrt((a.x-b.x)**2 + (a.y-b.y)**2);
}

export function isThereAShipAtThisWaypoint(waypoint: Waypoint): boolean {
	return getShips().some(s => s.nav.waypointSymbol === waypoint.symbol);
}

export function sortByDistanceFrom<T extends Point>(a: Point, points: Array<T>): Array<{data: T, distance: number}>{
	let result = points.map(function (m) { return {
		data: m,
		distance: distance(a, m),
	}});
	result.sort(function(a, b) {
		if (a.distance < b.distance) {
			return -1;
		} else if (a.distance > b.distance) {
			return 1;
		}
		return 0;
	});
	return result;
}

type Step = {waypoint: Waypoint, prev: string, fuel: number, total: number};
type ShortestPath = Array<{symbol: string, fuel: number}>;

export async function shortestPath(origin: Waypoint, destination: Waypoint, range: number, waypoints: Array<Waypoint>): Promise<ShortestPath> {
	let backtrace: {[key: string]: Step} = {};
	let fuels: {[key: string]: number} = {}; // fuel = distance + 1 per hop
	let unvisited: {[key: string]: Waypoint} = {};
	waypoints.forEach(function(w) {
		fuels[w.symbol] = Infinity;
		unvisited[w.symbol] = w;
	});
	fuels[origin.symbol] = 0;
	let queue = new PriorityQueue();
	queue.enqueue({waypoint: origin, prev: origin.symbol, fuel: 0, total: 0}, 0);
	while(!queue.isEmpty()) {
		let step = queue.dequeue() as Step;
		const symbol = step.waypoint.symbol;
		if (!(symbol in unvisited)) continue;
		backtrace[symbol] = step;
		const prev = unvisited[symbol];
		delete unvisited[symbol];
		for(const ws in unvisited) {
			const w = unvisited[ws];
			const f = distance(prev, w) + 1;
			if (f > range) continue;
			const total = step.fuel + f;
			if (fuels[ws] > total) {
				fuels[ws] = total;
				const nextStep = {waypoint: w, prev: symbol, fuel: f, total: total};
				if (ws === destination.symbol) {
					backtrace[ws] = nextStep;
					break;
				}
				if (!w.traits.some(t => t.symbol === 'MARKETPLACE')) continue;
				const m = await market(w);
				if (whatCanBeTradedAt({"FUEL":1}, m.exports.concat(m.exchange)).length === 0) continue;
				queue.enqueue(nextStep, total);
			}
		}
	}
	let result: ShortestPath = [];
	let step = backtrace[destination.symbol];
	if (step === undefined) throw `Cannot compute shortest path from ${origin.symbol} to ${destination.symbol} with range ${range}.`;
	for (; step.waypoint.symbol != origin.symbol; step = backtrace[step.prev]) {
		result.push({symbol: step.waypoint.symbol, fuel: step.fuel});
	}
	return result;
}

export function systemFromWaypoint(waypoint: string): string {
	return waypoint.split('-').slice(0,2).join('-');
}

export function whatCanBeTradedAt(cargo: CargoManifest, goods: Array<CommonThing>): Array<CommonThing> {
    return goods.filter(g => cargo[g.symbol] !== undefined );
}
