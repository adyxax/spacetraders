import db from './db.js';

const addContractStatement = db.prepare(`INSERT INTO contracts(data) VALUES (json(?));`);
const getContractStatement = db.prepare(`SELECT data FROM contracts WHERE data->>'id' = ?;`);
const getContractsStatement = db.prepare(`SELECT data FROM contracts WHERE data->>'fulfilled' = false;`);
const updateContractStatement = db.prepare(`UPDATE contracts SET data = json(:data) WHERE data->>'id' = :id;`);

export function getContract(id) {
	const data = getContractStatement.get(id);
	if (data === undefined) {
		return null;
	}
	return JSON.parse(data.data);
}

export function getContracts() {
	const data = getContractsStatement.all();
	return data.map(contractData => JSON.parse(contractData.data));
}

export function setContract(data) {
	if (getContract(data.id) === null) {
		return addContractStatement.run(JSON.stringify(data)).lastInsertRowid;
	} else {
		return updateContractStatement.run({
			data: JSON.stringify(data),
			id: data.id,
		}).changes;
	}
}
