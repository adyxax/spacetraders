import * as automation from './automation/automation.js';
import * as autoContracting from './automation/contracting.js';
import * as autoMining from './automation/mining.js';
import * as api from './lib/api.js';
import * as contracts from './lib/contracts.js';
import * as ships from './lib/ships.js';
import * as systems from './lib/systems.js';

function usage() {
	console.log(`autoContractForShip [ship_symbol]	run a contract extraction-delivery loop for the ship.
contracts.contracts			List your contracts.
my-agent				Fetch your agent's status.
register [symbol] [faction]		Registers your agent then inits the database
ships.ship [ship_symbol]		Retrieve a ship's status.
ships					Retrieve all of your ships.
status					Servers' status`);
}

switch(process.argv[2]) {
case 'autoContractForShip':
	await autoContracting.auto({ship: process.argv[3]});
	break;
case 'autoMiningForShip':
	await autoMining.mineUntilFullOf({ship: process.argv[3], good: 'NON_EXISTENT'});
	break;
case 'my-agent':
	api.debugLog(await api.send({endpoint: '/my/agent'}));
	break;
case 'register':
	if (process.argv[3] !== undefined && process.argv[4] !== undefined) {
		automation.register(process.argv[3], process.argv[4]);
	} else {
		usage();
	}
	break;
case 'ships':
	api.debugLog(await api.send({endpoint: '/my/ships'}));
	break;
case 'status':
	api.debugLog(await api.send({endpoint: '/'}));
	break;
default:
	// wip and manual actions
	switch(process.argv[2]) {
	case 'contracts.accept':
		api.debugLog(await contracts.accept({contract: process.argv[3]}));
		break;
	case 'contracts.contracts':
		api.debugLog(await contracts.contracts());
		break;
	case 'contracts.fulfill':
		api.debugLog(await contracts.fulfill({contract: process.argv[3]}));
		break;
	case 'ships.dock':
		api.debugLog(await ships.dock({symbol: process.argv[3]}));
		break;
	case 'ships.jump':
		api.debugLog(await ships.jump({ship: process.argv[3], system: process.argv[4]}));
		break;
	//case 'market':
	//	api.send({endpoint: `/systems/${process.argv[3]}/waypoints/${process.argv[4]}/market`});
	//	break;
	case 'ships.navigate':
		api.debugLog(await ships.navigate({symbol: process.argv[3], waypoint: process.argv[4]}));
		break;
	case 'ships.negotiate':
		api.debugLog(await ships.negotiate({ship: process.argv[3]}));
		break;
	case 'ships.orbit':
		api.debugLog(await ships.orbit({symbol: process.argv[3]}));
		break;
	case 'ships.purchase':
		api.debugLog(await ships.purchase({shipType: process.argv[3], waypoint: process.argv[4]}));
		break;
	case 'ships.refuel':
		api.debugLog(await ships.refuel({symbol: process.argv[3]}));
		break;
	case 'ships.sell':
		api.debugLog(await ships.sell({symbol: process.argv[3], good: process.argv[4], units: process.argv[5]}));
		break;
	case 'ships.ship':
		api.debugLog(await ships.ship({symbol: process.argv[3]}));
		break;
	case 'ships.survey':
		api.debugLog(await ships.survey({symbol: process.argv[3]}));
		break;
	case 'systems.asteroids':
		api.debugLog(await systems.type({symbol: process.argv[3], type: 'ASTEROID_FIELD'}));
		break;
	case 'systems.jumpGate':
		api.debugLog(await systems.type({symbol: process.argv[3], type: 'JUMP_GATE'}));
		break;
	case 'systems.shipyard':
		api.debugLog(await systems.shipyard({symbol: process.argv[3]}));
		break;
	case 'systems.shipyards':
		api.debugLog(await systems.trait({symbol: process.argv[3], trait: 'SHIPYARD'}));
		break;
	case 'systems.system':
		api.debugLog(await systems.system({symbol: process.argv[3]}));
		break;
	case 'systems.waypoints':
		api.debugLog(await systems.waypoints({symbol: process.argv[3]}));
		break;
	default:
		usage();
	}
}
