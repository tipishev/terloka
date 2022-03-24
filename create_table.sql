CREATE TABLE search_requests (
	id INTEGER PRIMARY KEY AUTOINCREMENT
	position_id INTEGER NOT NULL,
	description TEXT NOT NULL,
	created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
	result TEXT,
	updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
);

INSERT INTO search_requests (position_id, description) VALUES (123, "проверка");

