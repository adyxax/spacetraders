import * as autoContracting from './automation/contracting.js';
import * as autoExploring from './automation/exploration.js';
import * as autoInit from './automation/init.js';
import * as api from './lib/api.js';
import * as contracts from './lib/contracts.js';

await autoInit.init();
autoContracting.init();
autoExploring.init();
