import * as agent from './lib/agent.js';
import * as api from './lib/api.js';
import * as ships from './lib/ships.js';

function usage() {
	console.log(`contracts\t\t\tList all of your contracts.
extract [ship] [good]\t\tExtracts a good from the asteroid field the ship is orbiting and sell locally the unwanted ores until cargo is full
init [symbol] [faction] [token]\tinits the database in case we have an already registered game
my-agent\t\t\tFetch your agent's details.
register [symbol] [faction]\tRegisters your agent then inits the database
ships\t\t\tRetrieve all of your ships.`);
}

switch(process.argv[2]) {
case 'contracts':
	api.send({ endpoint: '/my/contracts'});
	break;
case 'extract':
	if (process.argv[3] !== undefined && process.argv[4] !== undefined) {
		agent.extract({ship: process.argv[3], good: process.argv[4]});
	} else {
		usage();
	}
	break;
case 'init':
	if (process.argv[3] !== undefined && process.argv[4] !== undefined && process.argv[5] !== undefined) {
		agent.init(process.argv[3], process.argv[4], process.argv[5]);
	} else {
		usage();
	}
	break;
case 'my-agent':
	api.send({endpoint: '/my/agent'});
	break;
case 'register':
	if (process.argv[3] !== undefined && process.argv[4] !== undefined) {
		agent.register(process.argv[3], process.argv[4]);
	} else {
		usage();
	}
	break;
case 'ships':
	api.send({endpoint: '/my/ships'});
	break;
default:
	// wip and manual actions
	switch(process.argv[2]) {
	case 'contract-accept':
		api.send({endpoint: `/my/contracts/${process.argv[3]}/accept`, method: 'POST'});
		break;
	case 'dock':
		ships.dock({ship: process.argv[3]});
		break;
	case 'market':
		api.send({endpoint: `/systems/${process.argv[3]}/waypoints/${process.argv[4]}/market`});
		break;
	case 'navigate':
		ships.navigate({ship: process.argv[3], waypoint: process.argv[4]});
		break;
	case 'orbit':
		ships.orbit({ship: process.argv[3]});
		break;
	case 'purchase':
		api.send({endpoint: '/my/ships', method: 'POST', payload: {
			shipType: 'SHIP_MINING_DRONE',
			waypointSymbol: process.argv[3],
		}});
		break;
	case 'refuel':
		ships.refuel({ship: process.argv[3]});
		break;
	case 'sell':
		ships.sell({ship: process.argv[3], good: process.argv[4], units: process.argv[5]});
		break;
	case 'shipyard':
		api.send({endpoint: `/systems/${process.argv[3]}/waypoints/${process.argv[4]}/shipyard`});
		break;
	case 'waypoints':
		api.send({endpoint: `/systems/${process.argv[3]}/waypoints?limit=20&page=1`});
		break;
	default:
		usage();
	}
}
