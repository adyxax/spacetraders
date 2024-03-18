// cargo is a ship.cargo object, want is an optional symbol
export function categorizeCargo(cargo, want) {
    const wanted = cargo.inventory.filter(i => i.symbol === want || i.symbol === 'ANTIMATTER');
    const goods = cargo.inventory.filter(i => i.symbol !== want && i.symbol !== 'ANTIMATTER');
    const wobj = wanted.reduce(function(acc, e) {
	acc[e.symbol] = e.units;
	return acc;
    }, {});
    const gobj = goods.reduce(function(acc, e) {
	acc[e.symbol] = e.units;
	return acc;
    }, {});
    return {wanted: wobj, goods: gobj};
}

export function systemFromWaypoint(waypoint) {
	return waypoint.split('-').slice(0,2).join('-');
}
