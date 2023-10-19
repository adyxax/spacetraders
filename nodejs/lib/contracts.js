import * as dbAgents from '../database/agents.js';
import * as dbContracts from '../database/contracts.js';
import * as api from './api.js';
import * as dbShips from '../database/ships.js';
import * as libShips from '../lib/ships.js';

export async function accept(ctx) {
	const contract = dbContracts.getContract(ctx.id);
	if (contract.accepted) {
		return;
	}
	await api.send({endpoint: `/my/contracts/${ctx.id}/accept`, method: 'POST'});
	contract.accepted = true;
	dbContracts.setContract(contract);
}

export async function contracts() {
	const contracts = await api.send({endpoint: '/my/contracts', page: 1});
	contracts.forEach(contract => dbContracts.setContract(contract));
	return contracts;
}

// returns true if the contract has been fulfilled
export async function deliver(ctx) {
	const contract = dbContracts.getContract(ctx.id);
	if (contract.terms.deliver[0].unitsRequired === contract.terms.deliver[0].unitsFulfilled) {
		await fulfill(ctx);
		return true;
	}
	await libShips.dock(ctx);
	const response = await api.send({ endpoint: `/my/contracts/${ctx.id}/deliver`, method: 'POST', payload: {
		shipSymbol: ctx.symbol,
		tradeSymbol: ctx.good,
		units: ctx.units,
	}});
	if (response.error !== undefined) {
		switch(response.error.code) {
		case 4509: // contract delivery terms have been met
			await fulfill(ctx);
			return true;
		default: // yet unhandled error
			api.debugLog(response);
			throw response;
		}
	}
	dbContracts.setContract(response.data.contract);
	dbShips.setShipCargo(ctx.symbol, response.data.cargo);
	// TODO track credits
	if(response.data.contract.terms.deliver[0].unitsRequired === response.data.contract.terms.deliver[0].unitsFulfilled) {
		await fulfill(ctx);
		return true;
	}
	return false;
}

export async function fulfill(ctx) {
	const contract = dbContracts.getContract(ctx.id);
	if (contract.fulfilled) {
		return;
	}
	const response = await api.send({ endpoint: `/my/contracts/${ctx.id}/fulfill`, method: 'POST'});
	if (response.error !== undefined) {
		api.debugLog(response);
		throw response;
	}
	dbAgents.setAgent(response.data.agent);
	dbContracts.setContract(response.data.contract);
}
