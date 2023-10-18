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
CREATE UNIQUE INDEX contracts_data_fulfilled ON contracts (json_extract(data, '$.fulfilled'));
CREATE TABLE ships (
  id INTEGER PRIMARY KEY,
  data TEXT NOT NULL
);
CREATE UNIQUE INDEX ships_data_symbol ON ships (json_extract(data, '$.symbol'));
CREATE INDEX ships_data_nav_systemSymbol ON ships (json_extract(data, '$.nav.systemSymbol'));
CREATE TABLE systems (
  id INTEGER PRIMARY KEY,
  data TEXT NOT NULL
);
CREATE UNIQUE INDEX systems_data_symbol ON systems (json_extract(data, '$.symbol'));
