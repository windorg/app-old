alter table cards
  add owner_id uuid;
update cards
  set owner_id = (select user_id from boards where id = cards.board_id);
alter table cards alter column owner_id set not null;
CREATE INDEX cards_owner_id_index ON cards (owner_id);
ALTER TABLE cards
  ADD CONSTRAINT cards_ref_owner_id FOREIGN KEY (owner_id) REFERENCES users (id) ON DELETE CASCADE;

alter table card_updates
  add owner_id uuid;
update card_updates
  set owner_id = (select owner_id from cards where id = card_updates.card_id);
alter table card_updates alter column owner_id set not null;
CREATE INDEX card_updates_owner_id_index ON card_updates (owner_id);
ALTER TABLE card_updates
  ADD CONSTRAINT card_updates_ref_owner_id FOREIGN KEY (owner_id) REFERENCES users (id) ON DELETE CASCADE;

alter table boards
  rename column user_id to owner_id;
alter index boards_user_id_index rename to boards_owner_id_index;
alter table boards
  rename constraint boards_ref_user_id to boards_ref_owner_id;
