import * as agent from './lib/agent.js';
import * as api from './lib/api.js';
import * as contracts from './lib/contracts.js';
import * as ships from './lib/ships.js';
import * as systems from './lib/systems.js';

function usage() {
	console.log(`contracts\t\t\tList all of your contracts.
extract [ship] [good]\t\tExtracts a good from the asteroid field the ship is orbiting and sell locally the unwanted ores until cargo is full
init [symbol] [faction] [token]\tinits the database in case we have an already registered game
my-agent\t\t\tFetch your agent's details.
register [symbol] [faction]\tRegisters your agent then inits the database
ships\t\t\tRetrieve all of your ships.`);
}

switch(process.argv[2]) {
	break;
//case 'deliver':
//	agent.deliver({
//		contract: process.argv[3],
//		ship: process.argv[4],
//		good: process.argv[5],
//		destination: process.argv[6],
//		field: process.argv[7],
//		//units: process.argv[8],
//	});
//	break;
//case 'extract':
//	if (process.argv[3] !== undefined && process.argv[4] !== undefined) {
//		agent.extract({ship: process.argv[3], good: process.argv[4]});
//	} else {
//		usage();
//	}
//	break;
case 'init':
	if (process.argv[3] !== undefined && process.argv[4] !== undefined && process.argv[5] !== undefined) {
		agent.init(process.argv[3], process.argv[4], process.argv[5]);
	} else {
		usage();
	}
	break;
case 'my-agent':
	api.debugLog(await api.send({endpoint: '/my/agent'}));
	break;
case 'register':
	if (process.argv[3] !== undefined && process.argv[4] !== undefined) {
		agent.register(process.argv[3], process.argv[4]);
	} else {
		usage();
	}
	break;
case 'ships':
	api.debugLog(await api.send({endpoint: '/my/ships'}));
	break;
default:
	// wip and manual actions
	switch(process.argv[2]) {
	case 'contracts.contracts':
		api.debugLog(await contracts.contracts());
		break;
	case 'contracts.accept':
		api.debugLog(await contracts.accept({id: process.argv[3]}));
		break;
	case 'ships.dock':
		api.debugLog(await ships.dock({ship: process.argv[3]}));
		break;
	case 'ships.extract':
		api.debugLog(await ships.extract({ship: process.argv[3]}));
		break;
	//case 'market':
	//	api.send({endpoint: `/systems/${process.argv[3]}/waypoints/${process.argv[4]}/market`});
	//	break;
	case 'ships.navigate':
		api.debugLog(await ships.navigate({ship: process.argv[3], waypoint: process.argv[4]}));
		break;
	case 'ships.orbit':
		api.debugLog(await ships.orbit({ship: process.argv[3]}));
		break;
	case 'ships.purchase':
		api.debugLog(await ships.purchase({shipType: process.argv[3], waypoint: process.argv[4]}));
		break;
	case 'ships.refuel':
		api.debugLog(await ships.refuel({ship: process.argv[3]}));
		break;
	case 'ships.sell':
		api.debugLog(await ships.sell({ship: process.argv[3], good: process.argv[4], units: process.argv[5]}));
		break;
	case 'ships.ship':
		api.debugLog(await ships.ship({ship: process.argv[3]}));
		break;
	case 'systems.asteroids':
		api.debugLog(await systems.type({symbol: process.argv[3], type: 'ASTEROID_FIELD'}));
		break;
	case 'systems.shipyards':
		api.debugLog(await systems.trait({symbol: process.argv[3], trait: 'SHIPYARD'}));
		break;
	default:
		usage();
	}
}
