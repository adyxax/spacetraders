import fs from 'fs';
import Database from 'better-sqlite3';

const allMigrations = [
	'database/000_init.sql',
	'database/001_systems.sql',
	'database/002_ships.sql',
	'database/003_surveys.sql',
	'database/004_markets.sql',
];

const db = new Database(
	process.env.NODE_ENV === 'test' ? 'test.db' : 'spacetraders.db',
	process.env.NODE_ENV === 'development' ? { verbose: console.log } : undefined
);
db.pragma('foreign_keys = ON');
db.pragma('journal_mode = WAL');

function init() {
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

export function reset() {
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
