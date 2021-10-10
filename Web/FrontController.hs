module Web.FrontController where

import IHP.RouterPrelude
import Web.Controller.Prelude
import Web.View.Layout

import IHP.LoginSupport.Middleware
import Web.Controller.Sessions

import qualified Prelude

-- Controller Imports
import Web.Controller.Reply
import Web.Controller.User
import Web.Controller.CardUpdate
import Web.Controller.Card
import Web.Controller.Board
import Web.Controller.Inbox
import Web.Controller.Static

instance FrontController WebApplication where
    controllers = 
        [ startPage WelcomeAction
        -- Generator Marker
        , parseRoute @ReplyController
        , parseRoute @InboxController
        , parseRoute @UserController
        , parseRoute @CardUpdateController
        , parseRoute @CardController
        , parseRoute @BoardController
        , parseRoute @SessionsController
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
