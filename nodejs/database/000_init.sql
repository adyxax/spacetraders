CREATE TABLE schema_version (
	version INTEGER NOT NULL
);
CREATE TABLE config (
	id INTEGER PRIMARY KEY,
	key TEXT NOT NULL UNIQUE,
	value TEXT NOT NULL
);
