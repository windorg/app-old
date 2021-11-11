module Web.View.Feed.Show where

import Web.View.Prelude
import Web.Helper.View
import Web.ViewTypes

data FeedView = FeedView { feedItems :: [FeedItemV] }

instance View FeedView where
    html FeedView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={BoardsAction}>Boards</a></li>
                <li class="breadcrumb-item active">Feed</li>
            </ol>
        </nav>
        <h1>Feed</h1>
        <p class="text-muted">Showing updates from the last 14 days.</p>
        {forEach feedItems renderFeedItem}
    |]

renderFeedItem :: FeedItemV -> Html
renderFeedItem FeedItemCardUpdateV {..} = [hsx|
<div class="mt-5 woc-feed-item woc-feed-item-card-update">
  <strong>
    <a href={ShowUserAction (get #id owner)}>{get #displayName owner}</a> —
    {get #title card}
  </strong>
  <div style="margin-bottom:.3em">
    <span class="text-muted small">
      <a href={pathTo (ShowCardAction (get #id card)) <> "#comment-" <> show (get #id cardUpdate)}>
          {renderTimestamp (get #createdAt cardUpdate)}
      </a>
    </span>
    {when private lockIcon}
    <div class="ml-3 d-inline">
      {renderCardUpdateGoToCard cardUpdate}
    </div>
  </div>
  <div class="rendered-content">
    {renderMarkdown (get #content cardUpdate)}
  </div>
</div>
|]
  where
    private = case cardUpdate ^. #settings_ % #visibility of
      VisibilityPublic -> False
      VisibilityPrivate -> True

renderCardUpdateGoToCard cardUpdate = [hsx|
  <a class="btn btn-tiny btn-outline-secondary"
     href={ShowCardAction (get #cardId cardUpdate)}>
    Go to card
  </a>|]