{-# LANGUAGE AllowAmbiguousTypes #-}

module Web.Controller.Authorization where

import Web.Controller.Prelude
import Named
import Control.Monad.Extra (andM)
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

instance Access Board where
    userCanView boardId = do
        board <- fetch boardId
        case board ^. #settings_ % #visibility of
            VisibilityPublic -> pure True
            VisibilityPrivate -> pure $ mbCurrentUserId == Just (get #ownerId board)
    userCanEdit boardId = (== mbCurrentUserId) <$> (Just <$> getOwnerById @Board boardId)
    userCanDelete boardId = userCanEdit @Board boardId

instance Access Card where
    userCanView cardId = do
        card <- fetch cardId
        andM [
            -- Must be able to see the board
            userCanView @Board (get #boardId card),
            -- And the card, too
            case card ^. #settings_ % #visibility of
                VisibilityPublic -> pure True
                VisibilityPrivate -> pure $ mbCurrentUserId == Just (get #ownerId card)
            ]
    userCanEdit cardId = (== mbCurrentUserId) <$> (Just <$> getOwnerById @Card cardId)
    userCanDelete cardId = userCanEdit @Card cardId

instance Access CardUpdate where
    userCanView cardUpdateId = do
        cardUpdate <- fetch cardUpdateId
        andM [
            -- Must be able to see the card
            userCanView @Card (get #cardId cardUpdate),
            -- And the card update, too
            case cardUpdate ^. #settings_ % #visibility of
                VisibilityPublic -> pure True
                VisibilityPrivate -> pure $ mbCurrentUserId == Just (get #ownerId cardUpdate)
            ]
    userCanEdit cardUpdateId = (== mbCurrentUserId) <$> (Just <$> getOwnerById @CardUpdate cardUpdateId)
    userCanDelete cardUpdateId = userCanEdit @CardUpdate cardUpdateId

instance Access Reply where
    userCanView replyId = do
        reply <- fetch replyId
        andM [
            userCanView @CardUpdate (get #cardUpdateId reply),
            -- TODO: we might want to add replies that only the author & the card owner can see
            case reply ^. #settings_ % #visibility of
                VisibilityPublic -> pure True
                VisibilityPrivate -> (== mbCurrentUserId) <$> 
                    (Just <$> getOwnerById @CardUpdate (get #cardUpdateId reply))
            ]
    userCanEdit replyId = do
        reply :: Reply <- fetch replyId
        pure $ userCanEditReplyPure (#user mbCurrentUserId) reply
    userCanDelete replyId = do
        reply :: Reply <- fetch replyId
        cardOwner <- getOwnerById @CardUpdate (get #cardUpdateId reply)
        pure $ userCanDeleteReplyPure 
            (#user mbCurrentUserId)
            (#cardOwner cardOwner)
            reply
        
userCanReply :: (?context :: ControllerContext, ?modelContext :: ModelContext) => Id CardUpdate -> IO Bool
userCanReply id = userCanView @CardUpdate id