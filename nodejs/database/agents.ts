import { Agent } from '../model/agent.ts';
import db from './db.js';

const addAgentStatement = db.prepare(`INSERT INTO agents(data) VALUES (json(?));`);
const getAgentStatement = db.prepare(`SELECT data FROM agents;`);
const setAgentStatement = db.prepare(`UPDATE agents SET data = json(?);`);

export function addAgent(agent: Agent) {
	addAgentStatement.run(JSON.stringify(agent));
}

export function getAgent(): Agent|null {
	const data = getAgentStatement.get() as {data: string}|undefined;
	if (!data) return null;
	return JSON.parse(data.data);
}

export function setAgent(agent: Agent) {
	setAgentStatement.run(JSON.stringify(agent));
}
