module Web.Controller.Static where
import Web.Controller.Prelude
import Web.View.Static.Welcome

instance Controller StaticController where
    action WelcomeAction = if isJust currentUserOrNothing
        then redirectTo BoardsAction
        else render WelcomeView
