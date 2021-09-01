

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;


SET SESSION AUTHORIZATION DEFAULT;

ALTER TABLE public.users DISABLE TRIGGER ALL;

INSERT INTO public.users (id, email, handle, created_at, display_name, password_hash, locked_at, failed_login_attempts) VALUES ('d2893aea-dcc2-445a-bfab-722bb51020f4', 'artyom@artyom.me', 'artyom', '2021-08-15 23:41:08.567098+02', 'Artyom Kazak', 'sha256|17|S+r4RkJV+j5mdY5pZXMxCg==|Fm9tiAEKIaubskd9cgnXZTQ9+qfpb7FDWkQzGDg2Glg=', NULL, 0);


ALTER TABLE public.users ENABLE TRIGGER ALL;


ALTER TABLE public.boards DISABLE TRIGGER ALL;

INSERT INTO public.boards (id, title, user_id, created_at) VALUES ('af44c8c9-9d8d-49e5-bb31-3d970ef02e0b', 'Everything', 'd2893aea-dcc2-445a-bfab-722bb51020f4', '2021-08-15 23:41:40.738802+02');


ALTER TABLE public.boards ENABLE TRIGGER ALL;


ALTER TABLE public.cards DISABLE TRIGGER ALL;

INSERT INTO public.cards (id, title, board_id, created_at) VALUES ('a4d244eb-0623-4c74-859d-21012b718e7d', 'Clean up the mess on the table', 'af44c8c9-9d8d-49e5-bb31-3d970ef02e0b', '2021-08-16 01:42:27.340191+02');
INSERT INTO public.cards (id, title, board_id, created_at) VALUES ('fde6dedf-cde3-4dc3-96ab-08498e63de63', 'wind of change: just going to see what happens', 'af44c8c9-9d8d-49e5-bb31-3d970ef02e0b', '2021-08-15 23:42:05.46303+02');
INSERT INTO public.cards (id, title, board_id, created_at) VALUES ('17d4cc38-66b1-4558-a74a-e5daa0a12893', 'Test', 'af44c8c9-9d8d-49e5-bb31-3d970ef02e0b', '2021-09-02 00:22:28.869145+02');


ALTER TABLE public.cards ENABLE TRIGGER ALL;


ALTER TABLE public.card_updates DISABLE TRIGGER ALL;

INSERT INTO public.card_updates (id, created_at, content, card_id) VALUES ('97d75128-7f66-4d73-9ff6-a1f7fcfce6e1', '2021-08-15 23:42:40.267145+02', 'Streaming a development session on Twitch', 'fde6dedf-cde3-4dc3-96ab-08498e63de63');
INSERT INTO public.card_updates (id, created_at, content, card_id) VALUES ('0186997f-69be-492a-b497-994976f6424b', '2021-08-16 01:24:22.371039+02', 'The Twitch stream is still going! Implemented a lot of stuff.', 'fde6dedf-cde3-4dc3-96ab-08498e63de63');
INSERT INTO public.card_updates (id, created_at, content, card_id) VALUES ('8dcf6165-065c-4701-9965-53d41ccfd458', '2021-08-16 02:26:47.914866+02', 'Okay, the Twitch stream is over. No viewers, but I learned a few lessons (shouldn''t use 480p, etc) and maybe in a few more streams there''ll be some viewers.

Also, posted on Twitter and asked (in English) if anybody would be interested in a stream. We''ll see tomorrow.', 'fde6dedf-cde3-4dc3-96ab-08498e63de63');
INSERT INTO public.card_updates (id, created_at, content, card_id) VALUES ('cb21c3a0-f9cc-4afb-baa4-49c1676d5198', '2021-08-16 05:43:00.588747+02', 'Spent a ton of time on ''autosize'', don''t like this. But decided to give up after all! Could have spent another few hours, maybe.', 'fde6dedf-cde3-4dc3-96ab-08498e63de63');
INSERT INTO public.card_updates (id, created_at, content, card_id) VALUES ('11026625-1117-4124-ba32-70beed3723b0', '2021-08-16 06:01:39.033102+02', 'Cleaned up a bit', 'a4d244eb-0623-4c74-859d-21012b718e7d');
INSERT INTO public.card_updates (id, created_at, content, card_id) VALUES ('48702749-b2d2-49a0-b906-21917676f363', '2021-08-16 18:06:38.446239+02', 'Done', 'a4d244eb-0623-4c74-859d-21012b718e7d');
INSERT INTO public.card_updates (id, created_at, content, card_id) VALUES ('a178637f-08a5-401c-a206-1e1b0a3be402', '2021-08-24 20:27:08.39942+02', 'Doing another wind of change stream, in English this time.', 'fde6dedf-cde3-4dc3-96ab-08498e63de63');
INSERT INTO public.card_updates (id, created_at, content, card_id) VALUES ('785c360b-1966-49a6-9a1e-4527d4aef7b5', '2021-09-02 00:22:32.363782+02', 'Something else', '17d4cc38-66b1-4558-a74a-e5daa0a12893');
INSERT INTO public.card_updates (id, created_at, content, card_id) VALUES ('d5377255-7b56-4972-ae15-265c0a8625e1', '2021-09-02 00:22:34.867182+02', 'Whatever', '17d4cc38-66b1-4558-a74a-e5daa0a12893');


ALTER TABLE public.card_updates ENABLE TRIGGER ALL;


