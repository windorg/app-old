-- | We often want to give extra information to the views.
module Web.ViewTypes where

import IHP.Prelude
import IHP.ModelSupport
import Generated.Types
import Application.Orphans
import Web.Controller.Authorization
import IHP.ControllerPrelude

data ReplyV = ReplyV {
    reply :: Reply,
    authorDisplayName :: Maybe Text,
    editable :: Bool,
    deletable :: Bool
}

fetchReplyV :: (?modelContext::ModelContext, ?context::ControllerContext) => Reply -> IO ReplyV
fetchReplyV reply = do
    mbAuthor <- mapM fetch (get #authorId reply)
    let authorDisplayName = get #displayName <$> mbAuthor
    editable <- userCanEdit @Reply (get #id reply)
    deletable <- userCanDelete @Reply (get #id reply)
    pure ReplyV{..}