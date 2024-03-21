import { Contract } from '../model/contract.ts';
import db from './db.ts';

const addContractStatement = db.prepare(`INSERT INTO contracts(data) VALUES (json(?));`);
const getContractStatement = db.prepare(`SELECT data FROM contracts WHERE data->>'id' = ?;`);
const getContractsStatement = db.prepare(`SELECT data FROM contracts WHERE data->>'fulfilled' = false;`);
const updateContractStatement = db.prepare(`UPDATE contracts SET data = json(:data) WHERE data->>'id' = :id;`);

export function getContract(id: string): Contract|null {
	const data = getContractStatement.get(id) as {data: string}|undefined;
	if (!data) return null;
	return JSON.parse(data.data);
}

export function getContracts(): Array<Contract> {
	const data = getContractsStatement.all() as Array<{data: string}>;
	return data.map(contractData => JSON.parse(contractData.data));
}

export function setContract(data: Contract) {
	if (getContract(data.id) === null) {
		addContractStatement.run(JSON.stringify(data));
	} else {
		updateContractStatement.run({
			data: JSON.stringify(data),
			id: data.id,
		});
	}
}
