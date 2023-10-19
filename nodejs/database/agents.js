import db from './db.js';

const addAgentStatement = db.prepare(`INSERT INTO agents(data) VALUES (json(?));`);
const getAgentStatement = db.prepare(`SELECT data FROM agents;`);
const setAgentStatement = db.prepare(`UPDATE agents SET data = json(?);`);

export function addAgent(agent) {
	return addAgentStatement.run(JSON.stringify(agent)).lastInsertRowid;
}

export function getAgent() {
	const data = getAgentStatement.get();
	if (data === undefined) {
		return null;
	}
	return JSON.parse(data.data);
}

export function setAgent(agent) {
	return setAgentStatement.run(JSON.stringify(agent)).changes;
}
