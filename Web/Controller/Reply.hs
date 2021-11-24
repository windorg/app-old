module Web.Controller.Reply where

import Data.Functor (void)
import qualified Data.Set as Set
import Debug.Trace (traceShowId)
import qualified Optics
import Web.Controller.Authorization
import Web.Controller.Prelude
import Web.Helper.Common
import Web.Helper.ReplySource
import Web.View.Reply.Edit
import Web.View.Reply.New
import Prelude (read)

instance (Controller CardController, Controller InboxController) => Controller ReplyController where
    action NewReplyAction{cardUpdateId, replySourceSerialized} = do
        ensureIsUser -- will redirect to login when logged out
        accessDeniedUnless =<< userCanReply cardUpdateId
        let replySource = read (cs replySourceSerialized)
        let reply =
                (newRecord :: Reply)
                    |> set #cardUpdateId cardUpdateId
                    |> Optics.set
                        #settings_
                        ReplySettings
                            { visibility = VisibilityPublic
                            }
        setModal NewView{..}
        jumpToReplySource replySource
    action EditReplyAction{replySourceSerialized, replyId} = do
        accessDeniedUnless =<< userCanEdit @Reply replyId
        let replySource = read (cs replySourceSerialized)
        reply <- fetch replyId
        setModal EditView{..}
        jumpToReplySource replySource
    action UpdateReplyAction{replySourceSerialized, replyId} = do
        accessDeniedUnless =<< userCanEdit @Reply replyId
        let replySource = read (cs replySourceSerialized)
        reply <- fetch replyId
        reply
            |> buildReply
            |> ifValid \case
                Left reply -> do
                    setModal EditView{..}
                    jumpToReplySource replySource
                Right reply -> do
                    reply <- reply |> updateRecord
                    redirectToReplySource replySource
    action CreateReplyAction{cardUpdateId, replySourceSerialized} = do
        accessDeniedUnless =<< userCanReply cardUpdateId
        cardOwner <- getOwnerById @CardUpdate cardUpdateId
        let replySource = read (cs replySourceSerialized)
        let reply =
                (newRecord :: Reply)
                    |> set #cardUpdateId cardUpdateId
                    |> set #authorId (Just currentUserId)
        reply
            |> buildReply
            |> traceShowId
            |> ifValid \case
                Left reply -> do
                    setModal NewView{..}
                    jumpToReplySource replySource
                Right reply -> do
                    reply <- reply |> createRecord
                    cardUpdate <- fetch cardUpdateId
                    -- When replying to a thread: subscribe the user to that thread
                    cardUpdate <-
                        if (currentUserId /= cardOwner)
                            then
                                cardUpdate
                                    |> Optics.over #settings_ (#subscribers Optics.%~ Set.insert currentUserId)
                                    |> updateRecord
                            else pure cardUpdate
                    -- Send out notifications
                    let subscribers' = Set.insert cardOwner (cardUpdate ^. #settings_ % #subscribers)
                    forM_ subscribers' \subscriber -> do
                        -- ON DELETE CASCADE won't work on subscribers, so we just delete the subscriber when we know
                        -- the user doesn't exist anymore
                        subscriberExists <- query @User |> filterWhere (#id, subscriber) |> fetchExists
                        if subscriberExists
                            then when (subscriber /= currentUserId) do
                                let subscriptionUpdate =
                                        (newRecord :: SubscriptionUpdate)
                                            |> set #subscriberId subscriber
                                            |> set #updateKind SukReply
                                            -- TODO: not sure why we fill both cardUpdateId and replyId
                                            |> set #cardUpdateId (Just cardUpdateId)
                                            |> set #replyId (Just (get #id reply))
                                subscriptionUpdate <- subscriptionUpdate |> createRecord
                                pure ()
                            else
                                cardUpdate
                                    |> Optics.over #settings_ (#subscribers Optics.%~ Set.delete subscriber)
                                    |> updateRecord
                                    |> void
                    redirectToReplySource replySource
    action DeleteReplyAction{replySourceSerialized, replyId} = do
        accessDeniedUnless =<< userCanDelete @Reply replyId
        let replySource = read (cs replySourceSerialized)
        reply <- fetch replyId
        deleteRecord reply
        redirectToReplySource replySource
    action UpdateMarkReplyAsReadAction{replySourceSerialized, replyId} = do
        -- TODO: am I sure this should be userCanView? Probably not.
        accessDeniedUnless =<< userCanView @Reply replyId
        mbUpdate <-
            query @SubscriptionUpdate
                |> filterWhere (#subscriberId, currentUserId)
                |> filterWhere (#replyId, Just replyId)
                |> filterWhere (#updateKind, SukReply)
                |> fetchOneOrNothing
        forM_ mbUpdate \update ->
            update |> set #isRead True |> updateRecord
        let replySource = read (cs replySourceSerialized)
        redirectToReplySource replySource

buildReply reply =
    reply
        |> fill @'["content"]
        |> Optics.set
            #settings_
            ReplySettings
                { visibility = if paramOrDefault False "private" then VisibilityPrivate else VisibilityPublic
                }
