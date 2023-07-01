CREATE TABLE surveys (
       id INTEGER PRIMARY KEY,
       data JSON NOT NULL
);
CREATE INDEX surveys_data_symbol on surveys (json_extract(data, '$.symbol'));
CREATE INDEX surveys_data_expiration on surveys (json_extract(data, '$.expiration'));
