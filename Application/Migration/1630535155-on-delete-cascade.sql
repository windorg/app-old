alter table card_updates
drop constraint card_updates_ref_card_id,
add constraint card_updates_ref_card_id
   foreign key (card_id)
   references cards(id)
   on delete cascade;