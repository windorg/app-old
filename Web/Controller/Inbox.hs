module Web.Controller.Inbox where

import Web.Controller.Prelude
import Web.Controller.Authorization
import Web.View.Inbox.Show
import Web.ViewTypes
import Control.Monad ((<=<))

getUnreadReplies :: (?context::ControllerContext, ?modelContext :: ModelContext) => IO [Id Reply]
getUnreadReplies = do
  unreadReplyUpdates <- query @SubscriptionUpdate
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
