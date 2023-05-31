import * as db from '../database/systems.js';

// Retrieves all systems information, should be called only once after registering
export async function init() {
	if (db.isInit()) {
		return;
	}
	for (let page=1; true; ++page) {
		const response = await api.send({endpoint: `/systems?limit=20&page=${page}`, priority:100});
		if (response.error !== undefined) {
			throw response;
		}
		response.data.forEach(system => db.setSystem(system));
		if (response.meta.total <= response.meta.limit * page) {
			break;
		}
	}
	console.log('Finished retrieving all systems information');
	db.init();
}
