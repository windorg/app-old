module Web.Helper.ReplySource where

import Web.Controller.Prelude

jumpToReplySource 
  :: (?context :: ControllerContext, ?modelContext :: ModelContext,
      Controller CardController, Controller InboxController) 
  => ReplySource -> IO ()
jumpToReplySource = \case
  ReplySourceCard cardId -> jumpToAction (ShowCardAction cardId)
  ReplySourceInbox -> jumpToAction ShowInboxAction

redirectToReplySource 
  :: (?context :: ControllerContext, ?modelContext :: ModelContext,
      Controller CardController, Controller InboxController,
      AutoRoute CardController, AutoRoute InboxController) 
  => ReplySource -> IO ()
redirectToReplySource = \case
  ReplySourceCard cardId -> redirectTo (ShowCardAction cardId)
  ReplySourceInbox -> redirectTo ShowInboxAction

pathToReplySource 
  :: (AutoRoute CardController, AutoRoute InboxController) 
  => ReplySource -> Text
pathToReplySource = \case
  ReplySourceCard cardId -> pathTo (ShowCardAction cardId)
  ReplySourceInbox -> pathTo ShowInboxAction