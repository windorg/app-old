module Web.Controller.Sessions where

import Web.Controller.Prelude
import Web.View.Sessions.New
import qualified IHP.AuthSupport.Controller.Sessions as Sessions

instance Controller SessionsController where
    action NewSessionAction = Sessions.newSessionAction @User
    action CreateSessionAction = Sessions.createSessionAction @User
    action DeleteSessionAction = do
        case currentUserOrNothing of
            Just user -> logout user
            Nothing -> pure ()
        redirectTo BoardsAction

instance Sessions.SessionsControllerConfig User