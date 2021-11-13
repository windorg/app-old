module Web.View.Inbox.Show where

import Web.View.Prelude
import Web.Helper.View
import Web.ViewTypes

data InboxView = InboxView { unreadReplies :: [ReplyV] }

instance View InboxView where
    html InboxView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={BoardsAction}>Boards</a></li>
                <li class="breadcrumb-item active">Inbox</li>
            </ol>
        </nav>
        <h1 class="mb-4">Inbox</h1>
        <div class="woc-inbox">
          {forEach unreadReplies renderReply}
        </div>
    |]

renderReply :: ReplyV -> Html
renderReply replyV = [hsx|
<div class="reply media">
  {gravatar}
  <div class="media-body ml-2 mt-n2">
    <div class="mb-1">
      <span class="text-muted small">
        {authorName}
        <span>
            <a href={pathTo (ShowCardAction (get #cardId replyV)) <> "#reply-" <> show (get #id reply)}>
                {renderTimestamp createdAt}
            </a>
        </span>
        <!-- We won't render the "delete" button to not confuse people into thinking delete = mark as read -->
        <div class="ml-2 d-inline">
          {renderReplyMarkAsReadButton reply}
        </div> 
      </span>
    </div>
    <div class="rendered-content small">{renderMarkdown content}</div>
  </div>
</div>
|]
  where
    reply@Reply{..} = get #reply replyV
    authorName = case get #author replyV of
      Nothing -> [hsx|<span class="mr-2 font-weight-bold">[deleted]</span>|]
      Just author -> [hsx|
        <span class="mr-2 font-weight-bold">
          <a href={ShowUserAction (get #id author)}>{get #displayName author}</a>
        </span>
      |]
    gravatar = case get #author replyV of
      Nothing -> [hsx|<span>{gravatarSmall ""}</span>|]
      Just author -> [hsx|<a href={ShowUserAction (get #id author)}>{gravatarSmall (get #email author)}</a>|]

renderReplyMarkAsReadButton :: Reply -> Html
renderReplyMarkAsReadButton reply = [hsx|
  <form class="d-inline" method="POST" action={UpdateMarkReplyAsReadAction (get #id reply) (show replySource)}>
    <button class="btn btn-tiny btn-outline-info">Mark as read</button>
  </form>
  |]
  where
    replySource = ReplySourceInbox