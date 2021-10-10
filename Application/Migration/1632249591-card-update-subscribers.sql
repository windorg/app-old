update card_updates
set settings = jsonb_insert (settings, '{subscribers}', '[]'::jsonb);