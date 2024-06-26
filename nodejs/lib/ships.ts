import {
	Response,
	debugLog,
	send,
	sleep,
} from './api.ts';
import {
	MarketTradeVolumeError,
	ShipIsCurrentlyInTransitError,
	ShipIsStillOnCooldownError,
	ShipRequiresMoreFuelForNavigationError,
} from './errors.ts';
import { Agent, setAgent } from './agent.ts';
import { Contract } from './contracts.ts';
import * as libSystems from './systems.ts';
import {
	Cargo,
	Cooldown,
	Fuel,
	Nav,
	Registration,
	Waypoint,
} from './types.ts';
import {
	is_there_a_ship_at_this_waypoint,
	shortestPath,
	sortByPrice,
} from './utils.ts';

export class Ship {
	cargo: Cargo;
	cooldown: Cooldown;
	// crew
	// engine
	// frame
	fuel: Fuel;
	// modules
	// mounts
	nav: Nav;
	// reactor
	registration: Registration;
	symbol: string;
	constructor(ship: Ship) {
		this.cargo = ship.cargo;
		this.cooldown = ship.cooldown;
		this.fuel = ship.fuel;
		this.nav = ship.nav;
		this.registration = ship.registration;
		this.symbol = ship.symbol;
	}
	async dock(): Promise<void> {
		if (this.nav.status === 'DOCKED') return;
		const response = await send<{nav: Nav}>({endpoint: `/my/ships/${this.symbol}/dock`, method: 'POST'});
		if (response.error) {
			switch(response.error.code) {
				case 4214:
					const sicite = response.error.data as ShipIsCurrentlyInTransitError;
					await sleep(sicite.secondsToArrival * 1000);
					return await this.dock();
				default: // yet unhandled error
					debugLog(response);
					throw response;
			}
		}
		this.nav = response.data.nav;
	}
	async extract(): Promise<Cargo> {
		if (this.isFull()) return this.cargo;
		// TODO move to a suitable asteroid?
		// const asteroidFields = await systems.type({symbol: this.nav.systemSymbol, type: 'ENGINEERED_ASTEROID'});
		// TODO if there are multiple fields, find the closest one?
		//await navigate({symbol: ctx.symbol, waypoint: asteroidFields[0].symbol});
		await this.orbit();
		// TODO handle surveying?
		const response = await send<{cooldown: Cooldown, cargo: Cargo}>({endpoint: `/my/ships/${this.symbol}/extract`, method: 'POST'}); // TODO extraction and events api response fields cf https://spacetraders.stoplight.io/docs/spacetraders/b3931d097608d-extract-resources
		if (response.error) {
			switch(response.error.code) {
				case 4000:
					const sisoce = response.error.data as ShipIsStillOnCooldownError;
					await sleep(sisoce.cooldown.remainingSeconds  * 1000);
					return await this.extract();
				case 4228: // ship is full
					return this.cargo;
				default: // yet unhandled error
					debugLog(response);
					throw response;
			}
		}
		this.cargo = response.data.cargo;
		await sleep(response.data.cooldown.remainingSeconds*1000);
		return this.cargo;
	}
	//async flightMode(mode: string): Promise<void> {
	//	if (this.nav.flightMode === mode) return;
	//	const response = await send<nav>({endpoint: `/my/ships/${this.symbol}/nav`, method: 'PATCH', payload: { flightmode: mode }});
	//	if (response.error) {
	//		switch(response.error.code) {
	//			case 4214:
	//				const sicite = response.error.data as ShipIsCurrentlyInTransitError;
	//				await sleep(sicite.secondsToArrival * 1000);
	//				return await this.flightMode(mode);
	//			default: // yet unhandled error
	//				debugLog(response);
	//				throw response;
	//		}
	//	}
	//	this.nav = response.data;
	//}
	isEmpty(): boolean {
		return this.cargo.inventory.some(i => i.symbol !== 'ANTIMATTER');
	}
	isFull(): boolean {
		return this.cargo.units >= this.cargo.capacity * 0.9;
	}
	async navigate(waypoint: Waypoint): Promise<void> {
		let path = await shortestPath(await libSystems.waypoint(this.nav.route.destination.symbol), waypoint, this.fuel.capacity, await libSystems.waypoints(this.nav.systemSymbol));
		while (path.length > 0) {
			const next = path.pop();
			if (next === undefined) break;
			if (next.fuel > this.fuel.current) {
				// TODO also refuel if the destination does not sell fuel?
				await this.refuel();
			}
			await this.navigateTo(next.symbol);
		}
	}
	private async navigateTo(symbol: string): Promise<void> {
		await this.orbit();
		//if (this.fuel.capacity === 0) this.flightMode('BURN');
		const response = await send<{fuel: Fuel, nav: Nav}>({endpoint: `/my/ships/${this.symbol}/navigate`, method: 'POST', payload: { waypointSymbol: symbol }}); // TODO events field
		if (response.error) {
			switch(response.error.code) {
				case 4203: // not enough fuel
					// This should not happen given the logic in navigate()
					const srmffne = response.error.data as ShipRequiresMoreFuelForNavigationError;
					debugLog(response);
					debugLog(srmffne);
					throw response;
				case 4214:
					const sicite = response.error.data as ShipIsCurrentlyInTransitError;
					await sleep(sicite.secondsToArrival * 1000);
					return await this.navigateTo(symbol);
				default: // yet unhandled error
					debugLog(response);
					throw response;
			}
		}
		this.fuel = response.data.fuel;
		this.nav = response.data.nav;
		const delay = new Date(this.nav.route.arrival).getTime()  - new Date().getTime() ;
		await sleep(delay);
		this.nav.status = 'IN_ORBIT'; // we arrive in orbit
	}
	async negotiate(): Promise<Contract> {
		await this.dock();
		const response = await send<{contract: Contract}>({endpoint: `/my/ships/${this.symbol}/negotiate/contract`, method: 'POST'});
		if (response.error) {
			switch(response.error.code) {
				case 4214:
					const sicite = response.error.data as ShipIsCurrentlyInTransitError;
					await sleep(sicite.secondsToArrival * 1000);
					return await this.negotiate();
				default: // yet unhandled error
					debugLog(response);
					throw response;
			}
		}
		return new Contract(response.data.contract);
	}
	async orbit(): Promise<void> {
		if (this.nav.status === 'IN_ORBIT') return;
		const response = await send<{nav: Nav}>({endpoint: `/my/ships/${this.symbol}/orbit`, method: 'POST'});
		if (response.error) {
			switch(response.error.code) {
				case 4214:
					const sicite = response.error.data as ShipIsCurrentlyInTransitError;
					await sleep(sicite.secondsToArrival * 1000);
					return await this.orbit();
				default: // yet unhandled error
					debugLog(response);
					throw response;
			}
		}
		this.nav = response.data.nav;
	}
	async purchase(tradeSymbol: string, units: number): Promise<void> {
		if (units <= 0) return;
		await this.dock();
		// TODO take into account the tradevolume, we might need to buy in multiple steps
		const response = await send<{agent: Agent, cargo: Cargo}>({endpoint: `/my/ships/${this.symbol}/purchase`, method: 'POST', payload: { symbol: tradeSymbol, units: units }}); // TODO transaction field
		if (response.error) {
			switch(response.error.code) {
				case 4604: // units per transaction limit exceeded
					const mtve = response.error.data as MarketTradeVolumeError;
					await this.purchase(tradeSymbol, mtve.tradeVolume);
					return await this.purchase(tradeSymbol, units - mtve.tradeVolume);
				default:
					debugLog(response);
					throw response;
			}
		}
		this.cargo = response.data.cargo;
		setAgent(response.data.agent);
	}
	async refuel(): Promise<void> {
		if (this.fuel.current === this.fuel.capacity) return;
		// TODO check if our current waypoint has a marketplace (and sells fuel)?
		await this.dock();
		const response = await send<{agent: Agent, fuel: Fuel}>({endpoint: `/my/ships/${this.symbol}/refuel`, method: 'POST'}); // TODO transaction field
		if (response.error) {
			debugLog(response);
			throw response;
		}
		this.fuel = response.data.fuel;
		setAgent(response.data.agent);
	}
	async sell(tradeSymbol: string, maybeUnits?: number): Promise<Cargo> {
		// TODO check if our current waypoint has a marketplace and buys tradeSymbol?
		await this.dock();
		let units = 0;
		if (maybeUnits !== undefined) {
			units = maybeUnits;
		} else {
			this.cargo.inventory.forEach(i => {if (i.symbol === tradeSymbol) units = i.units; });
		}
		// TODO take into account the tradevolume if we know it already, we might need to buy in multiple steps
		const response = await send<{agent: Agent, cargo: Cargo}>({endpoint: `/my/ships/${this.symbol}/sell`, method: 'POST', payload: { symbol: tradeSymbol, units: units }}); // TODO transaction field
		if (response.error) {
			switch(response.error.code) {
				case 4604: // units per transaction limit exceeded
					const mtve = response.error.data as MarketTradeVolumeError;
					await this.sell(tradeSymbol, mtve.tradeVolume); // TODO cache this information
					return await this.sell(tradeSymbol, units - mtve.tradeVolume);
				default:
					debugLog(response);
					throw response;
			}
		}
		this.cargo = response.data.cargo;
		setAgent(response.data.agent);
		return this.cargo;
	}
}

let myShips: Array<Ship> = [];

export function getShips(): Array<Ship> {
	return myShips;
}

export async function initShips(): Promise<void> {
	const response = await send<Array<Ship>>({endpoint: `/my/ships`, page: 1});
	if (response.error) {
		debugLog(response);
		throw response;
	}
	myShips = response.data.map(ship => new Ship(ship));
}

export async function purchaseShip(shipType: string): Promise<Ship> {
	const shipyardWaypoints = await libSystems.trait(getShips()[0].nav.systemSymbol, 'SHIPYARD');
	// backup candidates exist in case we do not have a probe in orbit of a
	// shipyard selling ${shipType}
	let backupCandidates: Array<{price: number, waypoint: Waypoint}> = [];
	let candidates: Array<{price: number, waypoint: Waypoint}> = [];
	for (const w of shipyardWaypoints) {
		const shipyardData = await libSystems.shipyard(w);
		const data = shipyardData.ships.filter(t => t.type === shipType);
		if (data.length === 0) continue;
		backupCandidates.push({price: data[0].purchasePrice, waypoint: w });
		if (!is_there_a_ship_at_this_waypoint(w)) continue;
		candidates.push({price: data[0].purchasePrice, waypoint: w });
	}
	let needsNavigate = false;
	if (candidates.length === 0) {
		if (backupCandidates.length === 0) throw `no shipyards sell ships of type ${shipType}`;
		candidates = backupCandidates;
		needsNavigate = true;
	}
	sortByPrice(candidates);
	if (needsNavigate) {
		// we did not have a probe in orbit of a shipyard selling ${shipType}
		// yet, must be early game buying our second probe so let's move the
		// starting probe in position
		await getShips()[1].navigate(candidates[0].waypoint);
	}
	const response = await send<{agent: Agent, ship: Ship}>({endpoint: `/my/ships`, method: 'POST', payload: {shipType: shipType, waypointSymbol: candidates[0].waypoint.symbol}}); // TODO transaction field
	if (response.error) {
		debugLog(response);
		throw response;
	}
	setAgent(response.data.agent);
	const ship = new Ship(response.data.ship);
	myShips.push(ship);
	return ship;
}
