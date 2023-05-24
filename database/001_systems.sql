CREATE TABLE systems (
       id INTEGER PRIMARY KEY,
       symbol TEXT NOT NULL UNIQUE,
       data TEXT NOT NULL,
       updated DATE DEFAULT (datetime('now'))
);
