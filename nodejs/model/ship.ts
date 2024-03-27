import { Cargo } from './cargo.ts';

export type Cooldown = {
	shipSymbol: string;
	totalSeconds: number;
	remainingSeconds: number;
};

export type Consummed = {
	amount: number;
	timestamp: Date;
};

export type Fuel = {
	capacity: number;
	consummed: Consummed;
	current: number;
};

export type Nav = {
	flightMode: string;
	route: Route;
	status: string;
	systemSymbol: string;
	waypointSymbol: string;
};

export type Registration = {
	factionSymbol: string;
	name: string;
	role: string;
};

export type Route = {
	arrival: Date;
	departureTime: Date;
	destination: RouteEndpoint;
	origin: RouteEndpoint;
};

export type RouteEndpoint = {
	type: string;
	symbol: string;
	systemSymbol: string;
	x: number;
	y: number;
};

export type Ship = {
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
};
