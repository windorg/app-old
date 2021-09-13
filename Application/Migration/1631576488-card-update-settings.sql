alter table card_updates
add settings jsonb default '{"visibility":"public"}' not null;

alter table card_updates
alter column settings drop default;