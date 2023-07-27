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
CREATE TABLE ships (
  id INTEGER PRIMARY KEY,
  data TEXT NOT NULL,
  available DATE NOT NULL
);
CREATE UNIQUE INDEX ships_data_symbol ON ships (json_extract(data, '$.symbol'));
CREATE TABLE systems (
  id INTEGER PRIMARY KEY,
  data TEXT NOT NULL
);
CREATE UNIQUE INDEX systems_data_symbol ON systems (json_extract(data, '$.symbol'));
