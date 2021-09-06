{-# LANGUAGE AllowAmbiguousTypes #-}

module Web.Controller.Authorization where

import Web.Controller.Prelude
import Named

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
    userCanView cardUpdateId = pure True
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
    userCanView replyId = pure True
    userCanEdit replyId = do
        reply :: Reply <- fetch replyId
        pure $ userCanEditReplyPure (#user (get #id <$> currentUserOrNothing)) reply
    userCanDelete replyId = do
        reply :: Reply <- fetch replyId
        [Only cardOwner] <- sqlQuery 
            "select user_id from boards where id = \
            \(select board_id from cards where id = \
            \(select card_id from card_updates where id = ?))"
            (Only (get #cardUpdateId reply))
        pure $ userCanDeleteReplyPure 
            (#user (get #id <$> currentUserOrNothing))
            (#cardOwner cardOwner)
            reply
        
userCanReply :: (?context :: ControllerContext, ?modelContext :: ModelContext) => Id CardUpdate -> IO Bool
userCanReply id = userCanView @CardUpdate id