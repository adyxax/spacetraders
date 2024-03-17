CREATE TABLE markets (
       id INTEGER PRIMARY KEY,
       system TEXT NOT NULL,
       data JSON NOT NULL
);
CREATE INDEX markets_system on markets (system);
CREATE UNIQUE INDEX markets_symbol on markets(json_extract(data, '$.symbol'));
