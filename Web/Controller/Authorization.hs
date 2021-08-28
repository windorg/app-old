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
