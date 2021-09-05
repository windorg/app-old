{-# LANGUAGE AllowAmbiguousTypes #-}

module Web.Controller.Authorization where

import Web.Controller.Prelude

class Access thing where
    userCanView :: (?context :: ControllerContext, ?modelContext :: ModelContext) => Id thing -> IO Bool
    userCanEdit :: (?context :: ControllerContext, ?modelContext :: ModelContext) => Id thing -> IO Bool

instance Access CardUpdate where
    userCanView cardUpdateId = pure True
    userCanEdit cardUpdateId = do
        cardUpdate <- fetch cardUpdateId
        card <- fetch (get #cardId cardUpdate)
        board <- fetch (get #boardId card)
        pure $ Just (get #userId board) == (get #id <$> currentUserOrNothing)

instance Access Card where
    userCanView cardId = pure True
    userCanEdit cardId = do
        card <- fetch cardId
        board <- fetch (get #boardId card)
        pure $ Just (get #userId board) == (get #id <$> currentUserOrNothing)

instance Access Board where
    userCanView boardId = pure True
    userCanEdit boardId = do
        board <- fetch boardId
        pure $ Just (get #userId board) == (get #id <$> currentUserOrNothing)

instance Access Reply where
    userCanView replyId = pure True
    userCanEdit replyId = do
        reply <- fetch replyId
        pure $ case get #authorId reply of
            Nothing -> False -- nobody can edit deleted users' replies
            Just authorId -> Just authorId == (get #id <$> currentUserOrNothing)
    -- TODO there should be userCanDelete b/c for replies edit /= delete. Or maybe for replies we want "hide" and not "delete", actually. Or something.
        
userCanReply :: (?context :: ControllerContext, ?modelContext :: ModelContext) => Id CardUpdate -> IO Bool
userCanReply id = userCanView @CardUpdate id