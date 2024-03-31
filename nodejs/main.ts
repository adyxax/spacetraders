import * as autoContracting from './automation/contracting.ts';
//import * as autoExploring from './automation/exploration.ts';
import * as autoInit from './automation/init.ts';
//import * as api from './lib/api.ts';
//import * as contracts from './lib/contracts.ts';

await autoInit.init();

await autoContracting.init();
//autoExploring.init();
