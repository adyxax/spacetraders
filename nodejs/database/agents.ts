import { Agent } from '../lib/types.ts';
import { DbData, db } from './db.ts';

const addAgentStatement = db.prepare(`INSERT INTO agents(data) VALUES (json(?));`);
const getAgentStatement = db.prepare(`SELECT data FROM agents;`);
const setAgentStatement = db.prepare(`UPDATE agents SET data = json(?);`);

export function addAgent(agent: Agent): void {
	addAgentStatement.run(JSON.stringify(agent));
}

export function getAgent(): Agent|null {
	const data = getAgentStatement.get() as DbData|undefined;
	if (!data) return null;
	return JSON.parse(data.data);
}

export function setAgent(agent: Agent): void {
	setAgentStatement.run(JSON.stringify(agent));
}
