-- | We often want to give extra information to the views.
module Web.ViewTypes where

import IHP.Prelude
import IHP.ModelSupport
import Generated.Types
import Application.Orphans
import Web.Controller.Authorization
import IHP.ControllerPrelude
import Web.Helper.Common

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
    ownerId <- getOwnerById @CardUpdate (get #cardUpdateId reply)
    mbAuthor <- mapM fetch (get #authorId reply)
    let authorDisplayName = get #displayName <$> mbAuthor
    editable <- userCanEdit @Reply (get #id reply)
    deletable <- userCanDelete @Reply (get #id reply)
    let markAsReadAble = get #isRead reply == False && Just ownerId == mbCurrentUserId
    pure ReplyV{..}