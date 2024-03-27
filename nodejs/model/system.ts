import { CommonThing } from 'common.ts';

export type Chart = {
	waypointSymbol: string;
	submittedBy: string;
	submittedOn: Date;
};

export type System = {
	symbol: string;
	sectorSymbol: string;
	type: string;
	x: number;
	y: number;
	waypoints: Array<Waypoint>;
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
