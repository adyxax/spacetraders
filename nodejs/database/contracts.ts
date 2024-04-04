import { Contract } from '../lib/types.ts';
import { DbData, db } from './db.ts';

const addContractStatement = db.prepare(`INSERT INTO contracts(data) VALUES (json(?));`);
const getContractStatement = db.prepare(`SELECT data FROM contracts WHERE data->>'id' = ?;`);
const getContractsStatement = db.prepare(`SELECT data FROM contracts WHERE data->>'fulfilled' = false;`);
const updateContractStatement = db.prepare(`UPDATE contracts SET data = json(:data) WHERE data->>'id' = :id;`);

export function getContract(id: string): Contract {
	const data = getContractStatement.get(id) as DbData|undefined;
	if (!data) throw `invalid id ${id} in getContract database call`;
	return JSON.parse(data.data);
}

export function getContracts(): Array<Contract> {
	const data = getContractsStatement.all() as Array<DbData>;
	return data.map(contractData => JSON.parse(contractData.data));
}

export function setContract(data: Contract): void {
	const changes = updateContractStatement.run({
		data: JSON.stringify(data),
		id: data.id,
	}).changes;
	if (changes === 0) addContractStatement.run(JSON.stringify(data));
}
