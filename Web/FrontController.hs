module Web.FrontController where

import IHP.RouterPrelude
import Web.Controller.Prelude
import Web.View.Layout

import IHP.LoginSupport.Middleware
import Web.Controller.Sessions

import qualified Prelude

-- Controller Imports

import Web.Controller.Board
import Web.Controller.Card
import Web.Controller.CardUpdate
import Web.Controller.Feed
import Web.Controller.Inbox
import Web.Controller.Reply
import Web.Controller.Static
import Web.Controller.User

instance FrontController WebApplication where
    controllers =
        [ startPage WelcomeAction,
          -- Generator Marker
          parseRoute @ReplyController,
          parseRoute @InboxController,
          parseRoute @FeedController,
          parseRoute @UserController,
          parseRoute @CardUpdateController,
          parseRoute @CardController,
          parseRoute @BoardController,
          parseRoute @LoginController
        ]

instance InitControllerContext WebApplication where
    -- Runs on every request apparently
    initContext = do
        initAuthentication @User
        inboxCount <- case currentUserOrNothing of
            Nothing -> pure Nothing
            -- TODO: this might be slow but we'll see later
            Just _ -> Just . Prelude.length <$> getUnreadReplies
        setLayout (defaultLayout LayoutView{..})
        initAutoRefresh
