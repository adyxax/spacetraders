import {
	Agent,
	Cargo,
} from './types.ts';
import {
	APIError,
	debugLog,
	send,
	sendPaginated,
} from './api.ts';
import { Ship } from './ships.ts';
import * as dbAgents from '../database/agents.ts';

export async function getContracts(): Promise<Array<Contract>> {
	const response = await sendPaginated<Contract>({endpoint: '/my/contracts'});
	return response.map(contract => new Contract(contract));
}

export class Contract {
	accepted: boolean;
	deadlineToAccept: Date;
	expiration: Date;
	factionSymbol: string;
	fulfilled: boolean;
	id: string;
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
	type: string;
	constructor(contract: Contract) {
		this.accepted = contract.accepted;
		this.deadlineToAccept = contract.deadlineToAccept;
		this.expiration = contract.expiration;
		this.factionSymbol = contract.factionSymbol;
		this.fulfilled = contract.fulfilled;
		this.id = contract.id;
		this.terms = contract.terms;
		this.type = contract.type;
	}
	async accept(): Promise<void> {
		if (this.accepted) return;
		const response = await send<{agent: Agent, contract: Contract, type: ''}>({endpoint: `/my/contracts/${this.id}/accept`, method: 'POST'});
		if (response.error) {
			debugLog(response);
			throw response;
		}
		dbAgents.setAgent(response.data.agent);
	}
	async deliver(ship: Ship): Promise<void> {
		const unitsRemaining = this.terms.deliver[0].unitsRequired - this.terms.deliver[0].unitsFulfilled;
		if (unitsRemaining <= 0) return await this.fulfill();
		const tradeSymbol = this.terms.deliver[0].tradeSymbol;
		let units = 0;
		ship.cargo.inventory.forEach(i => {if (i.symbol === tradeSymbol) units = i.units; });
		if (units === 0) return;
		if (units > unitsRemaining) units = unitsRemaining;
		await ship.dock(); // we need to be docked to deliver
		const response = await send<{contract: Contract, cargo: Cargo}>({ endpoint: `/my/contracts/${this.id}/deliver`, method: 'POST', payload: {
			shipSymbol: ship.symbol,
			tradeSymbol: tradeSymbol,
			units: units,
		}});
		if (response.error) {
			switch(response.error.code) {
				case 4503: // contract has expired
					// TODO sell cargo? the next trading loop should take care of it by itself
					this.fulfilled = true;
					return;
				case 4509: // contract delivery terms have been met
					return await this.fulfill();
				default: // yet unhandled error
					debugLog(response);
					throw response;
			}
		}
		ship.cargo = response.data.cargo;
		if(response.data.contract.terms.deliver[0].unitsRequired <= response.data.contract.terms.deliver[0].unitsFulfilled) {
			return await this.fulfill();
		}
	}
	async fulfill(): Promise<void> {
		if (this.terms.deliver[0].unitsRequired > this.terms.deliver[0].unitsFulfilled) return;
		if (this.fulfilled) return;
		const response = await send<{agent: Agent, contract: Contract}>({ endpoint: `/my/contracts/${this.id}/fulfill`, method: 'POST'});
		if (response.error) {
			debugLog(response);
			throw response;
		}
		dbAgents.setAgent(response.data.agent);
	}
};
