module Web.View.Feed.Show where

import Web.View.Prelude
import Web.Helper.View
import Web.ViewTypes

data FeedView = FeedView { 
  feedItems :: [FeedItemV],
  days :: Int
  }

instance View FeedView where
    html FeedView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={BoardsAction}>Boards</a></li>
                <li class="breadcrumb-item active">Feed</li>
            </ol>
        </nav>
        <h1>Feed</h1>
        <p class="text-muted">
          Showing updates from the last {if days == 1 then "day" else show days <> " days" :: Text}.
          {when (days < 14) show14}
        </p>
        {forEach feedItems renderFeedItem}
      |]
      where
        show14 = [hsx|<a href={ShowFeedAction (Just 14)}>Show the last 14 days.</a>|]

renderFeedItem :: FeedItemV -> Html
renderFeedItem FeedItemCardUpdateV {..} = [hsx|
<div class="mt-5 woc-feed-item woc-feed-item-card-update media">
  {gravatar}
  <div class="media-body ml-2 mt-n1">
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
    </div>
    <div class="rendered-content">
      {renderMarkdown (get #content cardUpdate)}
    </div>
  </div>
</div>
|]
  where
    private = case cardUpdate ^. #settings_ % #visibility of
      VisibilityPublic -> False
      VisibilityPrivate -> True
    gravatar = [hsx|<a href={ShowUserAction (get #id owner)}>{gravatarSmall (get #email owner)}</a>|]
