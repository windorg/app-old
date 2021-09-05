CREATE TABLE replies (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    content TEXT NOT NULL,
    card_update_id UUID NOT NULL,
    author_id UUID,
    is_read BOOLEAN DEFAULT false NOT NULL
);
CREATE INDEX replies_card_update_id_index ON replies (card_update_id);
CREATE INDEX replies_author_id_index ON replies (author_id);
ALTER TABLE replies ADD CONSTRAINT replies_ref_author_id FOREIGN KEY (author_id) REFERENCES users (id) ON DELETE SET NULL;
ALTER TABLE replies ADD CONSTRAINT replies_ref_card_update_id FOREIGN KEY (card_update_id) REFERENCES card_updates (id) ON DELETE CASCADE;