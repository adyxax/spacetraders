CREATE TABLE schema_version (
  version INTEGER NOT NULL
) STRICT;
CREATE TABLE agents (
  id INTEGER PRIMARY KEY,
  data TEXT NOT NULL
) STRICT;
CREATE TABLE systems (
  id INTEGER PRIMARY KEY,
  data TEXT NOT NULL
) STRICT;
CREATE UNIQUE INDEX systems_data_symbol ON systems (json_extract(data, '$.symbol'));
CREATE TABLE tokens (
  id INTEGER PRIMARY KEY,
  data TEXT NOT NULL
) STRICT;
