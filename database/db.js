import fs from 'fs';
import Database from 'better-sqlite3';

const allMigrations = [
	'database/000_init.sql',
	'database/001_systems.sql',
];

const db = new Database(
	process.env.NODE_ENV === 'test' ? 'test.db' : 'spacetraders.db',
	process.env.NODE_ENV === 'development' ? { verbose: console.log } : null
);
db.pragma('foreign_keys = ON');

db.transaction(function migrate() {
	let version;
	try {
		version = db.prepare('SELECT version FROM schema_version').get().version;
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

export default db;
