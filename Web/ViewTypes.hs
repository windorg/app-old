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
import Control.Monad (filterM)

----------------------------------------------------------------------------
-- CardV
----------------------------------------------------------------------------

data CardV = CardV {
    card :: Card,
    owner :: User,
    board :: Board,
    -- | Ordered in ascending order of creation
    cardUpdates :: [CardUpdateV],
    editable :: Bool
}

fetchCardV :: (?modelContext::ModelContext, ?context::ControllerContext) => Card -> IO CardV
fetchCardV card = do
    board <- fetch (get #boardId card)
    owner <- fetch (get #ownerId board)
    editable <- userCanEdit @Card (get #id card)
    cardUpdatesPlain :: [CardUpdate] <- 
        get #cardUpdates card 
            |> orderByDesc #createdAt
            |> fetch
            >>= filterM (userCanView @CardUpdate . get #id)
    cardUpdates :: [CardUpdateV] <- forM cardUpdatesPlain \cardUpdate -> do
        replies <- get #replies cardUpdate
            |> orderByAsc #createdAt
            |> fetch
            >>= filterM (userCanView @Reply . get #id)
            >>= mapM fetchReplyV
        -- Here we assume that the card update is editable iff the card is editable. Might not be true later.
        pure CardUpdateV {..}
    pure CardV {..}

----------------------------------------------------------------------------
-- CardUpdateV
----------------------------------------------------------------------------

-- TODO: might want to move ReplyV out of here
data CardUpdateV = CardUpdateV {
    cardUpdate :: CardUpdate,
    card :: Card,
    replies :: [ReplyV],
    editable :: Bool
}

----------------------------------------------------------------------------
-- ReplyV
----------------------------------------------------------------------------

data ReplyV = ReplyV {
    reply :: Reply,
    cardId :: Id Card,
    author :: Maybe User,
    editable :: Bool,
    deletable :: Bool,
    markAsReadAble :: Bool
}

fetchReplyV :: (?modelContext::ModelContext, ?context::ControllerContext) => Reply -> IO ReplyV
fetchReplyV reply = do
    cardUpdate <- fetch (get #cardUpdateId reply)
    let cardId = get #cardId cardUpdate
    let ownerId = get #ownerId cardUpdate
    author <- mapM fetch (get #authorId reply)
    editable <- userCanEdit @Reply (get #id reply)
    deletable <- userCanDelete @Reply (get #id reply)
    -- An inbox event that corresponds to the reply. If it exists, it can be marked as read.
    mbUpdate <- case mbCurrentUserId of
        Nothing -> pure Nothing
        Just currentUid -> query @SubscriptionUpdate
            |> filterWhere (#subscriberId, currentUid)
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
