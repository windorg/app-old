module Web.FrontController where

import IHP.RouterPrelude
import Web.Controller.Prelude
import Web.View.Layout (defaultLayout)

import IHP.LoginSupport.Middleware
import Web.Controller.Sessions

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
    initContext = do
        setLayout defaultLayout
        initAutoRefresh
        initAuthentication @User
