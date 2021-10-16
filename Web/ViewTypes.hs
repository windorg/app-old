-- | We often want to give extra information to the views.
module Web.ViewTypes where

import IHP.Prelude
import IHP.ModelSupport
import Generated.Types
import Application.Orphans
import Web.Controller.Authorization
import IHP.ControllerPrelude
import Web.Helper.Common
import Web.Types

----------------------------------------------------------------------------
-- ReplyV
----------------------------------------------------------------------------

data ReplyV = ReplyV {
    reply :: Reply,
    cardId :: Id Card,
    authorDisplayName :: Maybe Text,
    editable :: Bool,
    deletable :: Bool,
    markAsReadAble :: Bool
}

fetchReplyV :: (?modelContext::ModelContext, ?context::ControllerContext) => Reply -> IO ReplyV
fetchReplyV reply = do
    cardUpdate <- fetch (get #cardUpdateId reply)
    let cardId = get #cardId cardUpdate
    let ownerId = get #ownerId cardUpdate
    mbAuthor <- mapM fetch (get #authorId reply)
    let authorDisplayName = get #displayName <$> mbAuthor
    editable <- userCanEdit @Reply (get #id reply)
    deletable <- userCanDelete @Reply (get #id reply)
    -- An inbox event that corresponds to the reply. If it exists, it can be marked as read.
    mbUpdate <- query @SubscriptionUpdate
        |> filterWhere (#subscriberId, currentUserId)
        |> filterWhere (#replyId, Just (get #id reply))
        |> filterWhere (#updateKind, SukReply)
        |> filterWhere (#isRead, False)
        |> fetchOneOrNothing
    let markAsReadAble = isJust mbUpdate
    pure ReplyV{..}

----------------------------------------------------------------------------
-- FeedItemV
----------------------------------------------------------------------------

data FeedItemV
    = FeedItemCardUpdateV {
        owner :: User,
        cardUpdate :: CardUpdate,
        card :: Card
    }

fetchFeedItemV :: (?modelContext::ModelContext, ?context::ControllerContext) => FeedItem -> IO FeedItemV
fetchFeedItemV (FeedItemCardUpdate cardUpdate) = do
    card <- fetch (get #cardId cardUpdate)
    owner <- fetch (get #ownerId card)
    pure FeedItemCardUpdateV{..}
