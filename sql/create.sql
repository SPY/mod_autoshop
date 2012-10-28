CREATE SEQUENCE detalkupi.autoshop_items_seq;

CREATE TABLE detalkupi.autoshop_items (
	id integer NOT NULL DEFAULT NEXTVAL('detalkupi.autoshop_items_seq'),
	provider integer NOT NULL,
	keyprop varchar(100) NOT NULL,
	number varchar(50) NOT NULL,
	price real NOT NULL,
	title varchar(110) NOT NULL,
	delivery_time varchar(20),
	amount int DEFAULT 0,
	PRIMARY KEY (provider, number, keyprop)
);