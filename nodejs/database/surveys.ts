import { DbData, db } from './db.ts';

//const deleteExpiredSurveysStatement = db.prepare(`DELETE FROM surveys WHERE data->>'expiration' < ?;`);
//const getSurveysStatement = db.prepare(`SELECT data FROM surveys WHERE data->>'symbol' = ?;`);
//const setSurveysStatement = db.prepare(`INSERT INTO surveys(data) VALUES (json(?));`);
//
//export function deleteExpired() {
//	return deleteExpiredSurveysStatement.run(new Date().toISOString()).changes;
//}
//
//export function get(symbol) {
//	deleteExpired();
//	const data = getSurveysStatement.all(symbol);
//	return data.map(elt => JSON.parse(elt.data));
//}
//
//export function set(survey) {
//	deleteExpired();
//	return setSurveysStatement.run(JSON.stringify(survey));
//}
