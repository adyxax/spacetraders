import * as api from './api.js';

export async function extract(ctx) {
	const response = await api.send({endpoint: `/my/ships/${ctx.ship}/extract`, method: 'POST'});
	if (response.error !== undefined) {
		switch(response.error.code) {
		case 4000: // ship is on cooldown
			await api.sleep(response.error.data.cooldown.remainingSeconds  * 1000);
			return extract(ctx);
		case 4228: // ship is full
			return null;
		default: // yet unhandled error
			throw response;
		}
	} else {
		await api.sleep(response.data.cooldown.remainingSeconds*1000);
	}
	return response;
}

export async function dock(ctx) {
	const response = await api.send({endpoint: `/my/ships/${ctx.symbol}/dock`, method: 'POST'});
	if (response.error !== undefined) {
		switch(response.error.code) {
		case 4214: // ship is in transit
			await api.sleep(response.error.data.secondsToArrival  * 1000);
			return dock(ctx);
		default: // yet unhandled error
			throw response;
		}
	}
	return response;
}

export async function jump(ctx) {
	const response = await api.send({endpoint: `/my/ships/${ctx.ship}/jump`, method: 'POST', payload: { systemSymbol: ctx.system }});
	await api.sleep(response.data.cooldown.remainingSeconds*1000);
	return response;
}

export async function navigate(ctx) {
	const response = await api.send({endpoint: `/my/ships/${ctx.ship}/navigate`, method: 'POST', payload: { waypointSymbol: ctx.waypoint }});
	const delay = new Date(response.data.nav.route.arrival) - new Date();
	await api.sleep(delay);
	return response;
}

export async function negotiate(ctx) {
	return await api.send({endpoint: `/my/ships/${ctx.ship}/negotiate/contract`, method: 'POST'});
}

export async function orbit(ctx) {
	const response = await api.send({endpoint: `/my/ships/${ctx.symbol}/orbit`, method: 'POST'});
	if (response.error !== undefined) {
		switch(response.error.code) {
		case 4214: // ship is in transit
			await api.sleep(response.error.data.secondsToArrival  * 1000);
			return orbit(ctx);
		default: // yet unhandled error
			throw response;
		}
	}
	return response;
}

export async function purchase(ctx) {
	return await api.send({endpoint: '/my/ships', method: 'POST', payload: {
		shipType: ctx.shipType,
		waypointSymbol: ctx.waypoint,
	}});
}

export async function refuel(ctx) {
	return await api.send({endpoint: `/my/ships/${ctx.ship}/refuel`, method: 'POST'});
}

export async function sell(ctx) {
	return await api.send({endpoint: `/my/ships/${ctx.ship}/sell`, method: 'POST', payload: { symbol: ctx.good, units: ctx.units }});
}

export async function ship(ctx) {
	return await api.send({endpoint: `/my/ships/${ctx.ship}`});
}

export async function survey(ctx) {
	return await api.send({endpoint: `/my/ships/${ctx.ship}/survey`, method: 'POST'});
}
