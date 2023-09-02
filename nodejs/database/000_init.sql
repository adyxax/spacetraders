CREATE TABLE schema_version (
	version INTEGER NOT NULL
);
CREATE TABLE tokens (
	id INTEGER PRIMARY KEY,
	data TEXT NOT NULL
);
CREATE TABLE agents (
	id INTEGER PRIMARY KEY,
	data TEXT NOT NULL
);
CREATE TABLE contracts (
	id INTEGER PRIMARY KEY,
	data TEXT NOT NULL
);
CREATE UNIQUE INDEX contracts_data_id ON contracts (json_extract(data, '$.id'));
