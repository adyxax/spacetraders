import { Cooldown } from './types.ts';

export type ContractDeadlineExpired = {
	contractId: string;
	deadline: Date;
};

export type MarketTradeVolumeError = {
	waypointSymbol: string;
	tradeSymbol: string;
	units: number;
	tradeVolume: number;
};

export type RateLimitError = {
	type: string;
	retryAfter: number;
	limitBurst: number;
	limitPerSecond: number;
	remaining: number;
	reset: Date;
};

export type ShipIsCurrentlyInTransitError = {
	arrival: Date;
	departureSymbol: string;
	departureTime: Date;
	destinationSymbol: string;
	secondsToArrival: number;
};

export type ShipIsStillOnCooldownError = {
	cooldown: Cooldown;
};

export type ShipRequiresMoreFuelForNavigationError = {
	fuelAvailable: number;
	fuelRequired: number;
	shipSymbol: string;
};
