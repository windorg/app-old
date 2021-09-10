module Web.Controller.Inbox where

import Web.Controller.Prelude
import Web.Controller.Authorization
import Web.View.Inbox.Show
import Web.ViewTypes

instance Controller InboxController where
    beforeAction = ensureIsUser

    action ShowInboxAction = do
        unreadReplies <-
          mapM fetchReplyV =<<
          sqlQuery [sql|
            select * from replies
            where is_read = false and card_update_id in
                (select id from card_updates where card_id in
                   (select id from cards where board_id in
                      (select id from boards where user_id = ?)))
            order by created_at desc
            |]
            (Only currentUserId)
        render InboxView{..}
