CREATE TABLE systems (
       id INTEGER PRIMARY KEY,
       data JSON NOT NULL,
       updated DATE DEFAULT NULL
);
CREATE UNIQUE INDEX systems_data_symbol on systems (json_extract(data, '$.symbol'));
