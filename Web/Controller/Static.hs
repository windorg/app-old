module Web.Controller.Static where

import Web.Controller.Prelude
import Web.View.Static.Welcome
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types (status200)
import Network.Wai (responseLBS)

instance Controller StaticController where
    action WelcomeAction =
        if isJust currentUserOrNothing
            then redirectTo BoardsAction
            else renderLanding

renderLanding :: (?context :: ControllerContext) => IO () 
renderLanding = do
    page <- LBS.readFile "static/home.html"
    respondAndExit $ responseLBS status200 [(hContentType, "text/html")] page