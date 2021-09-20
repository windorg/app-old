alter table replies add settings jsonb default '{"visibility":"public"}' not null;
alter table replies alter column settings drop default;

alter table cards add settings jsonb default '{"visibility":"public"}' not null;
alter table cards alter column settings drop default;

alter table boards add settings jsonb default '{"visibility":"public"}' not null;
alter table boards alter column settings drop default;