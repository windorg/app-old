module Web.Routes where
import IHP.RouterPrelude
import Generated.Types
import Web.Types

-- Generator Marker
instance AutoRoute StaticController
instance AutoRoute BoardController


instance AutoRoute CardController


instance AutoRoute CardUpdateController


instance AutoRoute UserController

instance AutoRoute LoginController
instance AutoRoute ReplyController
instance AutoRoute InboxController
instance AutoRoute FeedController
