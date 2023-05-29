import * as autoContract from './automation/contract.js';
import * as autoMining from './automation/mining.js';
import * as agent from './lib/agent.js';
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
	await autoContract.auto({ship: process.argv[3]});
	break;
case 'autoMiningForShip':
	await autoMining.mineUntilFullOf({ship: process.argv[3], good: 'NON_EXISTENT'});
	break;
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
		api.debugLog(await ships.dock({ship: process.argv[3]}));
		break;
	case 'ships.extract':
		api.debugLog(await ships.extract({ship: process.argv[3]}));
		break;
	case 'ships.jump':
		api.debugLog(await ships.jump({ship: process.argv[3], system: process.argv[4]}));
		break;
	//case 'market':
	//	api.send({endpoint: `/systems/${process.argv[3]}/waypoints/${process.argv[4]}/market`});
	//	break;
	case 'ships.navigate':
		api.debugLog(await ships.navigate({ship: process.argv[3], waypoint: process.argv[4]}));
		break;
	case 'ships.negotiate':
		api.debugLog(await ships.negotiate({ship: process.argv[3]}));
		break;
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
	case 'ships.survey':
		api.debugLog(await ships.survey({ship: process.argv[3]}));
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
	case 'systems.systems':
		api.debugLog(await systems.systems());
		break;
	default:
		usage();
	}
}
