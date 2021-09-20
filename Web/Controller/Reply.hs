module Web.Controller.Reply where

import Web.Controller.Prelude
import Web.View.Reply.New
import Web.View.Reply.Edit
import Web.Helper.ReplySource
import Web.Controller.Authorization
import Prelude(read)
import Web.Helper.Common
import Debug.Trace (traceShowId)
import qualified Optics

instance (Controller CardController, Controller InboxController) => Controller ReplyController where
    action NewReplyAction { cardUpdateId, replySourceSerialized } = do
        accessDeniedUnless =<< userCanReply cardUpdateId
        let replySource = read (cs replySourceSerialized)
        let reply = (newRecord :: Reply)
                |> set #cardUpdateId cardUpdateId
                |> Optics.set #settings_ ReplySettings{
                       visibility = VisibilityPublic
                   }
        setModal NewView { .. }
        jumpToReplySource replySource

    action EditReplyAction { replySourceSerialized, replyId } = do
        accessDeniedUnless =<< userCanEdit @Reply replyId
        let replySource = read (cs replySourceSerialized)
        reply <- fetch replyId
        setModal EditView { .. }
        jumpToReplySource replySource

    action UpdateReplyAction { replySourceSerialized, replyId } = do
        accessDeniedUnless =<< userCanEdit @Reply replyId
        let replySource = read (cs replySourceSerialized)
        reply <- fetch replyId
        reply
            |> buildReply
            |> ifValid \case
                Left reply -> do
                    setModal EditView { .. }
                    jumpToReplySource replySource
                Right reply -> do
                    reply <- reply |> updateRecord
                    redirectToReplySource replySource

    action CreateReplyAction {cardUpdateId, replySourceSerialized} = do
        accessDeniedUnless =<< userCanReply cardUpdateId
        cardOwner <- getOwnerById @CardUpdate cardUpdateId
        let replySource = read (cs replySourceSerialized)
        let reply = (newRecord :: Reply)
              |> set #cardUpdateId cardUpdateId
              |> set #authorId (Just currentUserId)
              |> set #isRead (if currentUserId == cardOwner then True else False)
        reply
            |> buildReply
            |> traceShowId
            |> ifValid \case
                Left reply -> do
                    setModal NewView { .. }
                    jumpToReplySource replySource
                Right reply -> do
                    reply <- reply |> createRecord
                    redirectToReplySource replySource

    action DeleteReplyAction { replySourceSerialized, replyId } = do
        accessDeniedUnless =<< userCanDelete @Reply replyId
        let replySource = read (cs replySourceSerialized)
        reply <- fetch replyId
        deleteRecord reply
        redirectToReplySource replySource

    action UpdateMarkReplyAsReadAction { replySourceSerialized, replyId } = do
        accessDeniedUnless =<< userCanView @Reply replyId
        let replySource = read (cs replySourceSerialized)
        reply <- fetch replyId
        reply |> set #isRead True |> updateRecord
        redirectToReplySource replySource

buildReply reply = reply
    |> fill @'["content"]
    |> Optics.set #settings_ ReplySettings{
         visibility = if paramOrDefault False "private" then VisibilityPrivate else VisibilityPublic
       }
