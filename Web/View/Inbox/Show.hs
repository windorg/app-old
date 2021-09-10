module Web.View.Inbox.Show where

import Web.View.Prelude
import Web.ViewTypes

data InboxView = InboxView { unreadReplies :: [ReplyV] }

instance View InboxView where
    html InboxView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item active">Inbox</li>
            </ol>
        </nav>
        <h1>Inbox</h1>
        {forEach unreadReplies renderReply}
    |]

renderReply :: ReplyV -> Html
renderReply replyV = [hsx|
<div class="reply">
  <div class="mb-1">
    <span class="text-muted small">
      <span class="mr-2 font-weight-bold">{fromMaybe "[deleted]" (get #authorDisplayName replyV)}</span>
      <span><a href={ShowCardAction (get #cardId replyV)}>{renderTimestamp createdAt}</a></span>
      <!-- We won't render the "delete" button to not confuse people into thinking delete = mark as read -->
      <div class="ml-2 d-inline">
        {renderReplyMarkAsReadButton reply}
      </div> 
    </span>
  </div>
  <div class="rendered-content small">{renderMarkdown content}</div>
</div>
|]
  where
    reply@Reply{..} = get #reply replyV

renderReplyMarkAsReadButton :: Reply -> Html
renderReplyMarkAsReadButton reply = [hsx|
  <form class="d-inline" method="POST" action={UpdateMarkReplyAsReadAction (get #id reply) (show replySource)}>
    <button class="btn btn-tiny btn-outline-info">Mark as read</button>
  </form>
  |]
  where
    replySource = ReplySourceInbox