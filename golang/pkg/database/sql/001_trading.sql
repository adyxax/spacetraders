CREATE TABLE agents (
  id INTEGER PRIMARY KEY,
  data TEXT NOT NULL
);
CREATE TABLE markets (
  id INTEGER PRIMARY KEY,
  data JSON NOT NULL,
  updated DATE NOT NULL
);
CREATE UNIQUE INDEX markets_data_symbol on markets(json_extract(data, '$.symbol'));
CREATE TABLE shipyards (
       id INTEGER PRIMARY KEY,
       data JSON NOT NULL,
       updated DATE DEFAULT NULL
);
CREATE UNIQUE INDEX shipyards_data_symbol on shipyards (json_extract(data, '$.symbol'));
CREATE TABLE systems (
  id INTEGER PRIMARY KEY,
  data JSON NOT NULL
);
CREATE UNIQUE INDEX systems_data_symbol on systems (json_extract(data, '$.symbol'));

CREATE TABLE transactions (
  id INTEGER PRIMARY KEY,
  data JSON NOT NULL
);
CREATE UNIQUE INDEX transactions_data_symbol on transactions (json_extract(data, '$.symbol'));
CREATE INDEX transactions_data_type on transactions (json_extract(data, '$.type'));
CREATE INDEX transactions_data_shipSymbol on transactions (json_extract(data, '$.shipSymbol'));
CREATE INDEX transactions_data_waypointSymbol on transactions (json_extract(data, '$.waypointSymbol'));

CREATE TABLE waypoints (
  id INTEGER PRIMARY KEY,
  data JSON NOT NULL,
  updated DATE NOT NULL
);
CREATE UNIQUE INDEX waypoints_data_symbol on waypoints(json_extract(data, '$.symbol'));
