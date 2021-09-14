{-# LANGUAGE AllowAmbiguousTypes #-}

module Web.Controller.Authorization where

import Web.Controller.Prelude
import Named
import Web.Helper.Common

userCanEditReplyPure :: "user" :! Maybe (Id User) -> Reply -> Bool
userCanEditReplyPure (Arg user) reply =
    case get #authorId reply of
        Nothing -> False -- nobody can edit deleted users' replies
        Just authorId -> Just authorId == user

userCanDeleteReplyPure 
  :: "user" :! Maybe (Id User)
  -> "cardOwner" :! Id User
  -> Reply 
  -> Bool
userCanDeleteReplyPure (Arg user) (Arg cardOwner) reply =
    case user of
        Nothing -> False -- logged-out users can't delete anything
        Just u -> u == cardOwner || Just u == get #authorId reply
        
class Access thing where
    userCanView :: (?context :: ControllerContext, ?modelContext :: ModelContext) => Id thing -> IO Bool
    userCanEdit :: (?context :: ControllerContext, ?modelContext :: ModelContext) => Id thing -> IO Bool
    userCanDelete :: (?context :: ControllerContext, ?modelContext :: ModelContext) => Id thing -> IO Bool

instance Access CardUpdate where
    userCanView cardUpdateId = do
        cardUpdate <- fetch cardUpdateId
        case cardUpdate ^. #settings_ % #visibility of
            VisibilityPublic -> pure True
            VisibilityPrivate -> do
                card <- fetch (get #cardId cardUpdate)
                board <- fetch (get #boardId card)
                pure $ Just (get #userId board) == (get #id <$> currentUserOrNothing)
    userCanEdit cardUpdateId = do
        cardUpdate <- fetch cardUpdateId
        card <- fetch (get #cardId cardUpdate)
        board <- fetch (get #boardId card)
        pure $ Just (get #userId board) == (get #id <$> currentUserOrNothing)
    userCanDelete cardUpdateId = userCanEdit @CardUpdate cardUpdateId

instance Access Card where
    userCanView cardId = pure True
    userCanEdit cardId = do
        card <- fetch cardId
        board <- fetch (get #boardId card)
        pure $ Just (get #userId board) == (get #id <$> currentUserOrNothing)
    userCanDelete cardId = userCanEdit @Card cardId

instance Access Board where
    userCanView boardId = pure True
    userCanEdit boardId = do
        board <- fetch boardId
        pure $ Just (get #userId board) == (get #id <$> currentUserOrNothing)
    userCanDelete boardId = userCanEdit @Board boardId

instance Access Reply where
    -- Note: this ignores visibility settings for cards updates. If we ever have an API, we might want to rethink this.
    userCanView replyId = pure True
    userCanEdit replyId = do
        reply :: Reply <- fetch replyId
        pure $ userCanEditReplyPure (#user (get #id <$> currentUserOrNothing)) reply
    userCanDelete replyId = do
        reply :: Reply <- fetch replyId
        cardOwner <- getCardUpdateOwner (get #cardUpdateId reply)
        pure $ userCanDeleteReplyPure 
            (#user (get #id <$> currentUserOrNothing))
            (#cardOwner cardOwner)
            reply
        
userCanReply :: (?context :: ControllerContext, ?modelContext :: ModelContext) => Id CardUpdate -> IO Bool
userCanReply id = userCanView @CardUpdate id