CREATE TABLE followed_users (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    subscriber_id UUID NOT NULL,
    followed_user_id UUID NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
CREATE INDEX followed_users_subscriber_id_index ON followed_users (subscriber_id);
CREATE INDEX followed_users_followed_user_id_index ON followed_users (followed_user_id);
ALTER TABLE followed_users ADD CONSTRAINT followed_users_ref_followed_user_id FOREIGN KEY (followed_user_id) REFERENCES users (id) ON DELETE CASCADE;
ALTER TABLE followed_users ADD CONSTRAINT followed_users_ref_subscriber_id FOREIGN KEY (subscriber_id) REFERENCES users (id) ON DELETE CASCADE;
ALTER TABLE followed_users ADD CONSTRAINT followed_users_unique UNIQUE (subscriber_id, followed_user_id);