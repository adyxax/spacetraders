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
	contract = dbContracts.getContract(contract.id);
	if (contract.accepted) return contract;
	const response = await api.send<{agent: Agent, contract: Contract, type: ''}>({endpoint: `/my/contracts/${contract.id}/accept`, method: 'POST'});
	if (response.error) {
		api.debugLog(response);
		throw response;
	}
	dbAgents.setAgent(response.data.agent);
	dbContracts.setContract(response.data.contract);
	return response.data.contract;
}

export async function contracts(): Promise<Array<Contract>> {
	const response = await api.sendPaginated<Contract>({endpoint: '/my/contracts'});
	response.forEach(contract => dbContracts.setContract(contract));
	return response;
}

export async function deliver(contract: Contract, ship: Ship): Promise<Contract> {
	contract = dbContracts.getContract(contract.id);
	ship = dbShips.getShip(ship.symbol);
	if (contract.terms.deliver[0].unitsRequired >= contract.terms.deliver[0].unitsFulfilled) {
		return await fulfill(contract);
	}
	const tradeSymbol = contract.terms.deliver[0].tradeSymbol;
	let units = 0;
	ship.cargo.inventory.forEach(i => {if (i.symbol === tradeSymbol) units = i.units; });
	await libShips.dock(ship); // we need to be docked to deliver
	const response = await api.send<{contract: Contract, cargo: Cargo}>({ endpoint: `/my/contracts/${contract.id}/deliver`, method: 'POST', payload: {
		shipSymbol: ship.symbol,
		tradeSymbol: tradeSymbol,
		units: units,
	}});
	if (response.error) {
		switch(response.error.code) {
			case 4509: // contract delivery terms have been met
				return await fulfill(contract);
			default: // yet unhandled error
				api.debugLog(response);
				throw response;
		}
	}
	dbContracts.setContract(response.data.contract);
	dbShips.setShipCargo(ship.symbol, response.data.cargo);
	if(response.data.contract.terms.deliver[0].unitsRequired >= response.data.contract.terms.deliver[0].unitsFulfilled) {
		return await fulfill(response.data.contract);
	}
	return response.data.contract;
}

export async function fulfill(contract: Contract): Promise<Contract> {
	contract = dbContracts.getContract(contract.id);
	if (contract.fulfilled) return contract;
	const response = await api.send<{agent: Agent, contract: Contract}>({ endpoint: `/my/contracts/${contract.id}/fulfill`, method: 'POST'});
	if (response.error) {
		api.debugLog(response);
		throw response;
	}
	dbAgents.setAgent(response.data.agent);
	dbContracts.setContract(response.data.contract);
	return response.data.contract;
}
