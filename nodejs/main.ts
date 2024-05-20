import * as autoAgent from './automation/agent.ts';
//import * as autoExploring from './automation/exploration.ts';
import * as autoInit from './automation/init.ts';
import { getAgent } from './lib/agent.ts';
import { debugLog } from './lib/api.ts';

//debugLog(await send({endpoint: '/'}));

await autoInit.init();

debugLog(getAgent());

await autoAgent.run();

//import { market, trait } from './lib/systems.ts';
//const ws = await trait(ships[0].nav.systemSymbol, 'SHIPYARD');
//debugLog(ws);

//for (let w of ws) {
//	debugLog(await market(w));
//}
//
//await ships[0].navigate(await waypoint('X1-GR47-I59'));
