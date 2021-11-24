{-# LANGUAGE AllowAmbiguousTypes #-}

module Web.Helper.Common where

import Web.Controller.Prelude

mbCurrentUserId :: (?context :: ControllerContext) => _
mbCurrentUserId = get #id <$> currentUserOrNothing

class GetOwner model where
    getOwnerById :: (?modelContext :: ModelContext) => Id model -> IO (Id User)
    getOwner :: (?modelContext :: ModelContext) => model -> IO (Id User)

instance GetOwner CardUpdate where
    getOwnerById cardUpdateId = do
        [Only owner] <-
            sqlQuery
                [sql| select owner_id from card_updates where id = ? |]
                (Only cardUpdateId)
        pure owner
    getOwner cardUpdate = pure (get #ownerId cardUpdate)

instance GetOwner Card where
    getOwnerById cardId = do
        [Only owner] <-
            sqlQuery
                [sql| select owner_id from cards where id = ? |]
                (Only cardId)
        pure owner
    getOwner card = pure (get #ownerId card)

instance GetOwner Board where
    getOwnerById boardId = do
        [Only owner] <-
            sqlQuery
                [sql| select owner_id from boards where id = ? |]
                (Only boardId)
        pure owner
    getOwner board = pure (get #ownerId board)
