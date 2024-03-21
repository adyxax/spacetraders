export type Inventory = {
	description: string;
	name: string;
	symbol: string;
	units: number;
};

export type Cargo = {
	"capacity": number;
	"units": number;
	"inventory": Array<Inventory>;
};

// Custom type, not from space traders api
export type CargoManifest = {
	[key: string]: number;
};
