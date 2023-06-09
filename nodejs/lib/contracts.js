import * as api from './api.js';
import * as dbShips from '../database/ships.js';

export async function accept(ctx) {
	return await api.send({endpoint: `/my/contracts/${ctx.contract}/accept`, method: 'POST'});
}

export async function contracts() {
	return await api.send({endpoint: '/my/contracts'});
}

export async function deliver(ctx) {
	const response = await api.send({ endpoint: `/my/contracts/${ctx.contract}/deliver`, method: 'POST', payload: {
		shipSymbol: ctx.symbol,
		tradeSymbol: ctx.good,
		units: ctx.units,
	}});
	if (response.error !== undefined) {
		throw response;
	}
	dbShips.setShipCargo(ctx.symbol, response.data.cargo);
}

export async function fulfill(ctx) {
	return await api.send({ endpoint: `/my/contracts/${ctx.contract}/fulfill`, method: 'POST'});
}
