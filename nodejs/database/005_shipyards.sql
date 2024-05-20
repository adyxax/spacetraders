CREATE TABLE shipyards (
       id INTEGER PRIMARY KEY,
       data JSON NOT NULL,
       updated DATE DEFAULT NULL
);
CREATE UNIQUE INDEX shipyards_data_symbol on shipyards (json_extract(data, '$.symbol'));
