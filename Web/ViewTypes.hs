-- | We often want to give extra information to the views.
module Web.ViewTypes where

import Application.Orphans
import Control.Monad (filterM)
import GHC.Generics (Generic)
import Generated.Types
import IHP.ControllerPrelude
import IHP.ModelSupport
import IHP.Prelude
import Web.Controller.Authorization
import Web.Helper.Common
import Web.Types

----------------------------------------------------------------------------
-- CardV
----------------------------------------------------------------------------

data CardV = CardV
    { card :: Card,
      owner :: User,
      board :: Board,
      -- | Ordered in ascending order of creation
      cardUpdates :: [CardUpdateV],
      editable :: Bool
    }
    deriving (Generic)

fetchCardV :: (?modelContext :: ModelContext, ?context :: ControllerContext) => Card -> IO CardV
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
        replies <-
            get #replies cardUpdate
                |> orderByAsc #createdAt
                |> fetch
                >>= filterM (userCanView @Reply . get #id)
                >>= mapM fetchReplyV
        -- Here we assume that the card update is editable iff the card is editable. Might not be true later.
        pure CardUpdateV{..}
    pure CardV{..}

----------------------------------------------------------------------------
-- CardUpdateV
----------------------------------------------------------------------------

-- TODO: might want to move ReplyV out of here
data CardUpdateV = CardUpdateV
    { cardUpdate :: CardUpdate,
      card :: Card,
      replies :: [ReplyV],
      editable :: Bool
    }
    deriving (Generic)

----------------------------------------------------------------------------
-- ReplyV
----------------------------------------------------------------------------

data ReplyV = ReplyV
    { reply :: Reply,
      cardId :: Id Card,
      author :: Maybe User,
      editable :: Bool,
      deletable :: Bool,
      markAsReadAble :: Bool
    }
    deriving (Generic)

fetchReplyV :: (?modelContext :: ModelContext, ?context :: ControllerContext) => Reply -> IO ReplyV
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
        Just currentUid ->
            query @SubscriptionUpdate
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

data FeedItemV = FeedItemCardUpdateV
    { owner :: User,
      cardUpdate :: CardUpdate,
      card :: Card
    }
    deriving (Generic)

fetchFeedItemV :: (?modelContext :: ModelContext, ?context :: ControllerContext) => FeedItem -> IO FeedItemV
fetchFeedItemV (FeedItemCardUpdate cardUpdate) = do
    card <- fetch (get #cardId cardUpdate)
    owner <- fetch (get #ownerId card)
    pure FeedItemCardUpdateV{..}
