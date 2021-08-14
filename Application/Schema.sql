-- Your database schema. Use the Schema Designer at http://localhost:8001/ to add some tables.
CREATE TABLE users (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    email TEXT NOT NULL,
    handle TEXT NOT NULL UNIQUE
);
CREATE TABLE cards (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    title TEXT DEFAULT '' NOT NULL,
    comments JSONB[] DEFAULT '{}' NOT NULL,
    board_id UUID NOT NULL
);
CREATE TABLE boards (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    title TEXT DEFAULT '' NOT NULL,
    user_id UUID NOT NULL
);
CREATE INDEX boards_user_id_index ON boards (user_id);
ALTER TABLE boards ADD CONSTRAINT boards_ref_user_id FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE CASCADE;
ALTER TABLE cards ADD CONSTRAINT cards_ref_board_id FOREIGN KEY (board_id) REFERENCES boards (id) ON DELETE CASCADE;
