export type Agent = {
	accountId: string;
	credits: number;
	headquarters: string;
	shipCount: number;
	startingFaction: string;
	symbol: string;
};

export type CommonThing = {
	description: string;
	name: string;
	symbol: string;
};

export type Cargo = {
	capacity: number;
	units: number;
	inventory: Array<Inventory>;
};

// Custom type, not from space traders api
export type CargoManifest = {
	[key: string]: number;
};

export type Chart = {
	waypointSymbol: string;
	submittedBy: string;
	submittedOn: Date;
};

export type Contract = {
	id: string;
	factionSymbol: string;
	type: string;
	terms: {
		deadline: Date;
		payment: {
			onAccepted: number;
			onFulfilled: number;
		},
		deliver: Array<{
			tradeSymbol: string;
			destinationSymbol: string;
			unitsRequired: number;
			unitsFulfilled: number;
		}>;
	};
	accepted: boolean;
	fulfilled: boolean;
	expiration: Date;
	deadlineToAccept: Date;
};

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

export type Inventory = CommonThing & {
	units: number;
};

export type Market = {
	symbol: string;
	exchange: Array<CommonThing>;
	exports: Array<CommonThing>;
	imports: Array<CommonThing>;
	//transactions: Array<Transaction>;
	tradeGoods: Array<TradeGood>;
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

export type System = {
	symbol: string;
	sectorSymbol: string;
	type: string;
	x: number;
	y: number;
	waypoints: Array<Waypoint>;
};

export type TradeGood = CommonThing & {
	activity: string;
	purchasePrice: number;
	sellPrice: number;
	supply: string;
	tradeVolume: number;
	type: string;
};

export type Waypoint = {
	chart: Chart;
	factions: Array<{symbol: string;}>;
	isUnderConstruction: boolean;
	modifiers: Array<CommonThing>;
	orbitals: Array<{symbol: string;}>;
	orbits: string;
	symbol: string;
	traits: Array<CommonThing>;
	type: string;
	x: number;
	y: number;
};
