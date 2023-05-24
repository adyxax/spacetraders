import * as api from './api.js';

export async function accept(ctx) {
	return await api.send({endpoint: `/my/contracts/${ctx.id}/accept`, method: 'POST'});
}

export async function contracts() {
	return await api.send({endpoint: '/my/contracts'});
}

export async function deliver(ctx) {
	return await api.send({ endpoint: `/my/contracts/${ctx.contract}/deliver`, method: 'POST', payload: {
		shipSymbol: ctx.ship,
		tradeSymbol: ctx.good,
		units: ctx.units,
	}});
}
