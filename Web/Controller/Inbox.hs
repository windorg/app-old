module Web.Controller.Inbox where

import Control.Monad ((<=<))
import Web.Controller.Authorization
import Web.Controller.Prelude
import Web.View.Inbox.Show
import Web.ViewTypes

getUnreadReplies :: (?context :: ControllerContext, ?modelContext :: ModelContext) => IO [Id Reply]
getUnreadReplies = do
    ensureIsUser
    unreadReplyUpdates <-
        query @SubscriptionUpdate
            |> filterWhere (#subscriberId, currentUserId)
            |> filterWhere (#updateKind, SukReply)
            |> filterWhere (#isRead, False)
            |> orderByDesc #createdAt
            |> fetch
    let unreadReplyIds = mapMaybe (get #replyId) unreadReplyUpdates
    filterM (userCanView @Reply) unreadReplyIds

instance Controller InboxController where
    beforeAction = ensureIsUser

    action ShowInboxAction = do
        unreadReplies <- mapM (fetchReplyV <=< fetch @_ @Reply) =<< getUnreadReplies
        render InboxView{..}
