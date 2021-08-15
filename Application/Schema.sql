-- Your database schema. Use the Schema Designer at http://localhost:8001/ to add some tables.
CREATE TABLE users (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    email TEXT NOT NULL,
    handle TEXT NOT NULL UNIQUE,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
CREATE TABLE cards (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    title TEXT DEFAULT '' NOT NULL,
    board_id UUID NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
CREATE TABLE boards (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    title TEXT DEFAULT '' NOT NULL,
    user_id UUID NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
CREATE INDEX boards_user_id_index ON boards (user_id);
CREATE TABLE card_updates (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    content TEXT NOT NULL,
    card_id UUID NOT NULL
);
CREATE INDEX card_updates_card_id_index ON card_updates (card_id);
ALTER TABLE boards ADD CONSTRAINT boards_ref_user_id FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE CASCADE;
ALTER TABLE card_updates ADD CONSTRAINT card_updates_ref_card_id FOREIGN KEY (card_id) REFERENCES cards (id) ON DELETE NO ACTION;
ALTER TABLE cards ADD CONSTRAINT cards_ref_board_id FOREIGN KEY (board_id) REFERENCES boards (id) ON DELETE CASCADE;
