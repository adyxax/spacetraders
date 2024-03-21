import { Agent } from '../model/agent.ts';
import { APIError } from '../model/api.ts';
import { Cargo } from '../model/cargo.ts';
import { Contract } from '../model/contract.ts';
import { Ship } from '../model/ship.ts';
import * as dbAgents from '../database/agents.ts';
import * as dbContracts from '../database/contracts.ts';
import * as api from './api.ts';
import * as dbShips from '../database/ships.ts';
import * as libShips from '../lib/ships.ts';

export async function accept(contract: Contract): Promise<Contract> {
	if (contract.accepted) return contract;
	const response = await api.send<{agent: Agent, contract: Contract, type: ''}>({endpoint: `/my/contracts/${contract.id}/accept`, method: 'POST'});
	if ('apiError' in response) {
		api.debugLog(response);
		throw response;
	}
	dbAgents.setAgent(response.agent);
	dbContracts.setContract(response.contract);
	return response.contract;
}

export async function contracts(): Promise<Array<Contract>> {
	const response = await api.sendPaginated<Contract>({endpoint: '/my/contracts', page: 1});
	if ('apiError' in response) {
		api.debugLog(response);
		throw response;
	}
	response.forEach(contract => dbContracts.setContract(contract));
	return response;
}

export async function deliver(contract: Contract, ship: Ship): Promise<Contract> {
	if (contract.terms.deliver[0].unitsRequired >= contract.terms.deliver[0].unitsFulfilled) {
		return await fulfill(contract);
	}
	const tradeSymbol = contract.terms.deliver[0].tradeSymbol;
	let units = 0;
	ship.cargo.inventory.forEach(i => {if (i.symbol === tradeSymbol) units = i.units; });
	ship = await libShips.dock(ship); // we need to be docked to deliver
	const response = await api.send<{contract: Contract, cargo: Cargo}>({ endpoint: `/my/contracts/${contract.id}/deliver`, method: 'POST', payload: {
		shipSymbol: ship.symbol,
		tradeSymbol: tradeSymbol,
		units: units,
	}});
	if ('apiError' in response) {
		switch(response.code) {
			case 4509: // contract delivery terms have been met
				return await fulfill(contract);
			default: // yet unhandled error
				api.debugLog(response);
				throw response;
		}
	}
	dbContracts.setContract(response.contract);
	dbShips.setShipCargo(ship.symbol, response.cargo);
	if(response.contract.terms.deliver[0].unitsRequired >= response.contract.terms.deliver[0].unitsFulfilled) {
		return await fulfill(contract);
	}
	return response.contract;
}

export async function fulfill(contract: Contract): Promise<Contract> {
	if (contract.fulfilled) return contract;
	const response = await api.send<{agent: Agent, contract: Contract}>({ endpoint: `/my/contracts/${contract.id}/fulfill`, method: 'POST'});
	if ('apiError' in response) {
		api.debugLog(response);
		throw response;
	}
	dbAgents.setAgent(response.agent);
	dbContracts.setContract(response.contract);
	return response.contract;
}
