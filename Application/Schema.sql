-- Your database schema. Use the Schema Designer at http://localhost:8001/ to add some tables.
CREATE TABLE users (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    email TEXT NOT NULL,
    handle TEXT NOT NULL UNIQUE,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    display_name TEXT NOT NULL,
    password_hash TEXT NOT NULL,
    locked_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
    failed_login_attempts INT DEFAULT 0 NOT NULL
);
CREATE TABLE cards (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    title TEXT DEFAULT '' NOT NULL,
    board_id UUID NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    settings JSONB NOT NULL
);
CREATE TABLE boards (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    title TEXT DEFAULT '' NOT NULL,
    user_id UUID NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    settings JSONB NOT NULL
);
CREATE INDEX boards_user_id_index ON boards (user_id);
CREATE TABLE card_updates (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    content TEXT NOT NULL,
    card_id UUID NOT NULL,
    settings JSONB NOT NULL
);
CREATE INDEX card_updates_card_id_index ON card_updates (card_id);
CREATE TABLE replies (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    content TEXT NOT NULL,
    card_update_id UUID NOT NULL,
    author_id UUID,
    settings JSONB NOT NULL
);
CREATE INDEX replies_card_update_id_index ON replies (card_update_id);
CREATE INDEX replies_author_id_index ON replies (author_id);
CREATE TYPE subscription_update_kind AS ENUM ('suk_board', 'suk_card', 'suk_card_update', 'suk_reply');
CREATE TABLE subscription_updates (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    subscriber_id UUID NOT NULL,
    board_id UUID DEFAULT NULL,
    card_id UUID DEFAULT NULL,
    card_update_id UUID DEFAULT NULL,
    reply_id UUID DEFAULT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    update_kind subscription_update_kind NOT NULL,
    is_read BOOLEAN DEFAULT false NOT NULL
);
CREATE INDEX subscription_updates_card_update_id_index ON subscription_updates (card_update_id);
CREATE INDEX subscription_updates_subscriber_id_index ON subscription_updates (subscriber_id);
CREATE INDEX subscription_updates_card_id_index ON subscription_updates (card_id);
CREATE INDEX subscription_updates_board_id_index ON subscription_updates (board_id);
CREATE INDEX subscription_updates_reply_id_index ON subscription_updates (reply_id);
ALTER TABLE boards ADD CONSTRAINT boards_ref_user_id FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE CASCADE;
ALTER TABLE card_updates ADD CONSTRAINT card_updates_ref_card_id FOREIGN KEY (card_id) REFERENCES cards (id) ON DELETE CASCADE;
ALTER TABLE cards ADD CONSTRAINT cards_ref_board_id FOREIGN KEY (board_id) REFERENCES boards (id) ON DELETE CASCADE;
ALTER TABLE replies ADD CONSTRAINT replies_ref_author_id FOREIGN KEY (author_id) REFERENCES users (id) ON DELETE SET NULL;
ALTER TABLE replies ADD CONSTRAINT replies_ref_card_update_id FOREIGN KEY (card_update_id) REFERENCES card_updates (id) ON DELETE CASCADE;
ALTER TABLE subscription_updates ADD CONSTRAINT subscription_updates_ref_board_id FOREIGN KEY (board_id) REFERENCES boards (id) ON DELETE CASCADE;
ALTER TABLE subscription_updates ADD CONSTRAINT subscription_updates_ref_card_id FOREIGN KEY (card_id) REFERENCES cards (id) ON DELETE CASCADE;
ALTER TABLE subscription_updates ADD CONSTRAINT subscription_updates_ref_card_update_id FOREIGN KEY (card_update_id) REFERENCES card_updates (id) ON DELETE CASCADE;
ALTER TABLE subscription_updates ADD CONSTRAINT subscription_updates_ref_reply_id FOREIGN KEY (reply_id) REFERENCES replies (id) ON DELETE NO ACTION;
ALTER TABLE subscription_updates ADD CONSTRAINT subscription_updates_ref_subscriber_id FOREIGN KEY (subscriber_id) REFERENCES users (id) ON DELETE CASCADE;
