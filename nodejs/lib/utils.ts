import { Cargo, CargoManifest } from './types.ts';

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

export function systemFromWaypoint(waypoint: string): string {
	return waypoint.split('-').slice(0,2).join('-');
}
