module Web.View.Inbox.Show where

import Web.View.Prelude

data InboxView = InboxView { unreadReplies :: [Reply] }

instance View InboxView where
    html InboxView { .. } = [hsx|
        Inbox is coming.
    |]