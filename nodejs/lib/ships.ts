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
import {
	Agent,
	Cargo,
	Contract,
	Cooldown,
	Fuel,
	Nav,
	Registration,
	Waypoint,
} from './types.ts';
import * as dbAgents from '../database/agents.ts';

export async function getShips(): Promise<Array<Ship>> {
	const response = await send<Array<Ship>>({endpoint: `/my/ships`, page: 1});
	if (response.error) {
		debugLog(response);
		throw response;
	}
	return response.data.map(ship => new Ship(ship));
}

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
	isFull(): boolean {
		return this.cargo.units >= this.cargo.capacity * 0.9;
	}
	async navigate(waypoint: Waypoint): Promise<void> {
		if (this.nav.waypointSymbol === waypoint.symbol) return;
		// TODO compute fuel consumption and refuel if we do not have enough OR if the destination does not sell fuel?
		await this.refuel();
		await this.orbit();
		// TODO if we do not have enough fuel, make a stop to refuel along the way or drift to the destination
		const response = await send<{fuel: Fuel, nav: Nav}>({endpoint: `/my/ships/${this.symbol}/navigate`, method: 'POST', payload: { waypointSymbol: waypoint.symbol }}); // TODO events field
		if (response.error) {
			switch(response.error.code) {
				case 4203: // not enough fuel
					const srmffne = response.error.data as ShipRequiresMoreFuelForNavigationError;
					// TODO test if it exceeds our maximum
					// find an intermediate stop to refuel if that is the case
					debugLog(response);
					throw response;
					//await refuel(ship);
					//return await navigate(ship, waypoint);
				case 4214:
					const sicite = response.error.data as ShipIsCurrentlyInTransitError;
					await sleep(sicite.secondsToArrival * 1000);
					return await this.navigate(waypoint);
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
		return response.data.contract;
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
		dbAgents.setAgent(response.data.agent);
	}
	async refuel(): Promise<void> {
		// TODO check if our current waypoint has a marketplace (and sells fuel)?
		await this.dock();
		const response = await send<{agent: Agent, fuel: Fuel}>({endpoint: `/my/ships/${this.symbol}/refuel`, method: 'POST'}); // TODO transaction field
		if (response.error) {
			debugLog(response);
			throw response;
		}
		this.fuel = response.data.fuel;
		dbAgents.setAgent(response.data.agent);
	}
	async sell(tradeSymbol: string): Promise<Cargo> {
		// TODO check if our current waypoint has a marketplace and buys tradeSymbol?
		await this.dock();
		let units = 0;
		this.cargo.inventory.forEach(i => {if (i.symbol === tradeSymbol) units = i.units; });
		const response = await send<{agent: Agent, cargo: Cargo}>({endpoint: `/my/ships/${this.symbol}/sell`, method: 'POST', payload: { symbol: tradeSymbol, units: units }}); // TODO transaction field
		if (response.error) {
			debugLog(response);
			throw response;
		}
		this.cargo = response.data.cargo;
		dbAgents.setAgent(response.data.agent);
		return this.cargo;
	}
}
