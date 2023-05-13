import * as api from './api.js';

export function extract(ctx) {
	console.log(`${ctx.ship}: extracting`);
	api.send({endpoint: `/my/ships/${ctx.ship}/extract`, method: 'POST', next: ctx.next});
}

export function dock(ctx) {
	console.log(`${ctx.ship}: docking`);
	api.send({endpoint: `/my/ships/${ctx.ship}/dock`, method: 'POST', next: ctx.next});
}

export function navigate(ctx) {
	console.log(`${ctx.ship}: navigating to ${ctx.waypoint}`);
	api.send({endpoint: `/my/ships/${ctx.ship}/navigate`, method: 'POST', payload: { waypointSymbol: ctx.waypoint }, next: ctx.next});
}

export function orbit(ctx) {
	console.log(`${ctx.ship}: orbiting`);
	api.send({endpoint: `/my/ships/${ctx.ship}/orbit`, method: 'POST', next: ctx.next});
}

export function refuel(ctx) {
	console.log(`${ctx.ship}: refueling`);
	api.send({endpoint: `/my/ships/${ctx.ship}/refuel`, method: 'POST', next: ctx.next});
}

export function sell(ctx) {
	console.log(`${ctx.ship}: selling ${ctx.units} of ${ctx.good}`);
	api.send({endpoint: `/my/ships/${ctx.ship}/sell`, method: 'POST', payload: { symbol: ctx.good, units: ctx.units }, next: ctx.next});
}

export function ship(ctx) {
	api.send({endpoint: `/my/ships/${ctx.ship}`, next: ctx.next});
}
