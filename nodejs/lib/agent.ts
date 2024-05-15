import { debugLog, send } from './api.ts';

export class Agent {
	accountId: string;
	credits: number;
	headquarters: string;
	shipCount: number;
	startingFaction: string;
	symbol: string;
	constructor() {
		this.accountId = "";
		this.credits = 0;
		this.headquarters = "";
		this.shipCount = 0;
		this.startingFaction = "";
		this.symbol = "";
	}
	set(agent: Agent) {
		this.accountId = agent.accountId;
		this.credits = agent.credits;
		this.headquarters = agent.headquarters;
		this.shipCount = agent.shipCount;
		this.startingFaction = agent.startingFaction;
		this.symbol = agent.symbol;
	}
};

let myAgent : Agent = new Agent();

export function getAgent(): Agent {
	return myAgent;
}

export async function initAgent(): Promise<void> {
	const response = await send<Agent>({endpoint: `/my/agent`, page: 1});
	if (response.error) {
		debugLog(response);
		throw response;
	}
	myAgent.set(response.data);
}

export function setAgent(agent: Agent): void {
	myAgent.set(agent);
}
