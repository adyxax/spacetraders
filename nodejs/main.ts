import * as autoContracting from './automation/contracting.ts';
//import * as autoExploring from './automation/exploration.ts';
import * as autoInit from './automation/init.ts';
import { getAgent } from './lib/agent.ts';
import { getShips } from './lib/ships.ts';
import { debugLog, send } from './lib/api.ts';

//debugLog(await send({endpoint: '/'}));

await autoInit.init();
debugLog(getAgent());

debugLog(await getAgent().purchaseShip());

//const ships = getShips();
//await autoContracting.run(ships[0]); // dedicate the command ship to running contracts
//autoExploring.init();
