CREATE TABLE ships (
       id INTEGER PRIMARY KEY,
       data JSON NOT NULL,
       updated DATE DEFAULT NULL
);
CREATE UNIQUE INDEX ships_data_symbol ON ships (json_extract(data, '$.symbol'));
