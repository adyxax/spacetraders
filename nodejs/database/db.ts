import fs from 'fs';
import path from 'path';
import Database from 'better-sqlite3';

export type DbData = {data: string};

export const db = new Database(
	process.env.NODE_ENV === 'test' ? 'test.db' : 'spacetraders.db',
	process.env.NODE_ENV === 'development' ? { verbose: console.log } : undefined
);
db.pragma('foreign_keys = ON');
db.pragma('journal_mode = WAL');

function init(): void {
	const filenames = fs.readdirSync('./database/');
	const allMigrations = filenames.filter(e => e.match(/\.sql$/)).map(e => path.join('./database', e));
	db.transaction(function migrate() {
		let version;
		try {
			const res = db.prepare('SELECT version FROM schema_version').get() as {version: number};
			version = res.version;
		} catch {
			version = 0;
		}
		if (version === allMigrations.length) return;
		while (version < allMigrations.length) {
			db.exec(fs.readFileSync(allMigrations[version], 'utf8'));
			version++;
		}
		db.exec(`DELETE FROM schema_version; INSERT INTO schema_version (version) VALUES (${version});`);
	})();
}

export function reset(): void {
	const indices = db.prepare(`SELECT name FROM sqlite_master WHERE type = 'index';`).all() as Array<{name: string}>;
	const tables = db.prepare(`SELECT name FROM sqlite_master WHERE type = 'table';`).all() as Array<{name: string}>;
	const triggers = db.prepare(`SELECT name FROM sqlite_master WHERE type = 'trigger';`).all() as Array<{name: string}>;
	const views = db.prepare(`SELECT name FROM sqlite_master WHERE type = 'view';`).all() as Array<{name: string}>;
	indices.forEach(elt => db.exec(`DROP INDEX ${elt.name};`));
	tables.forEach(elt => db.exec(`DROP TABLE ${elt.name};`));
	triggers.forEach(elt => db.exec(`DROP TRIGGER ${elt.name};`));
	views.forEach(elt => db.exec(`DROP VIEW ${elt.name};`));
	db.exec(`VACUUM;`);
	init();
}

init();

export default db;
