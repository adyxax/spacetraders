CREATE TABLE ships (
       id INTEGER PRIMARY KEY,
       data JSON NOT NULL
);
CREATE UNIQUE INDEX ships_data_symbol ON ships (json_extract(data, '$.symbol'));
CREATE INDEX ships_data_nav_systemSymbol ON ships (json_extract(data, '$.nav.systemSymbol'));
