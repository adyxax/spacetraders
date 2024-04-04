import * as autoContracting from './automation/contracting.ts';
//import * as autoExploring from './automation/exploration.ts';
import * as autoInit from './automation/init.ts';
import { getShips } from './lib/ships.ts';

await autoInit.init();
const ships = await getShips();

await autoContracting.run(ships[0]); // dedicate the command ship to running contracts
//autoExploring.init();
