CREATE UNIQUE INDEX "users_email_key" ON "users"("email");

ALTER TABLE "subscription_updates" DROP CONSTRAINT "subscription_updates_ref_reply_id";

ALTER TABLE "subscription_updates" ADD CONSTRAINT "subscription_updates_ref_reply_id" FOREIGN KEY ("reply_id") REFERENCES "replies"("id") ON DELETE CASCADE ON UPDATE NO ACTION;
