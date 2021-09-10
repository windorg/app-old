module Web.FrontController where

import IHP.RouterPrelude
import Web.Controller.Prelude
import Web.View.Layout

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
    -- Runs on every request apparently
    initContext = do
        initAuthentication @User
        inboxCount <- case currentUserOrNothing of
            Nothing -> pure Nothing
            Just _ -> sqlQueryScalar [sql|
              select count(*) from replies
              where is_read = false and card_update_id in
                  (select id from card_updates where card_id in
                     (select id from cards where board_id in
                        (select id from boards where user_id = ?)))
              |]
              (Only currentUserId)
        setLayout (defaultLayout LayoutView{..})
        initAutoRefresh
