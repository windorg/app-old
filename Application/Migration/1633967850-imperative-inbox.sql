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

ALTER TABLE subscription_updates ADD CONSTRAINT subscription_updates_ref_board_id FOREIGN KEY (board_id) REFERENCES boards (id) ON DELETE CASCADE;
ALTER TABLE subscription_updates ADD CONSTRAINT subscription_updates_ref_card_id FOREIGN KEY (card_id) REFERENCES cards (id) ON DELETE CASCADE;
ALTER TABLE subscription_updates ADD CONSTRAINT subscription_updates_ref_card_update_id FOREIGN KEY (card_update_id) REFERENCES card_updates (id) ON DELETE CASCADE;
ALTER TABLE subscription_updates ADD CONSTRAINT subscription_updates_ref_reply_id FOREIGN KEY (reply_id) REFERENCES replies (id) ON DELETE NO ACTION;
ALTER TABLE subscription_updates ADD CONSTRAINT subscription_updates_ref_subscriber_id FOREIGN KEY (subscriber_id) REFERENCES users (id) ON DELETE CASCADE;


alter table replies drop column is_read;
