module Web.View.Card.Show where

import qualified Data.Text as T
import Named
import qualified Optics
import Web.Controller.Authorization
import Web.Helper.View
import Web.View.Prelude
import Web.ViewTypes
import qualified Prelude

data ShowView = ShowView
    { cardV :: CardV
    }

instance View ShowView where
    beforeRender ShowView{..} = do
        let owner = get #owner cardV
        setTitle (get #title (get #card cardV) <> " / wind of change")
        setOGTitle (get #title (get #card cardV))
        setDescription $ format "by {} @{}" (get #displayName owner) (get #handle owner)
        setOGDescription $ format "by {} @{}" (get #displayName owner) (get #handle owner)

    html ShowView{..} =
        [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={BoardsAction}>Boards</a></li>
                {userCrumb (($) #active False) (get #owner cardV)}
                {boardCrumb (($) #active False) (get #board cardV)}
                {cardCrumb (($) #active True) card}
            </ol>
        </nav>
        <h1 class="mb-4">
          {when archived archivedBadge}
          {when private lockIcon}
          {title}
          {when (get #editable cardV) 
            (renderCardEditButton card <> renderCardDeleteButton card)}
        </h1>
        {if reverseOrder then reverseOrderUpdates else normalOrderUpdates}
     |]
      where
        card@Card{..} = get #card cardV
        private = case card ^. #settings_ % #visibility of
            VisibilityPublic -> False
            VisibilityPrivate -> True
        reverseOrder = card ^. #settings_ % #reverseOrder
        archived = card ^. #settings_ % #archived
        (pinnedUpdates, otherUpdates) =
            partition (^. #cardUpdate % #settings_ % #pinned) (get #cardUpdates cardV)
        normalOrderUpdates =
            [hsx|
                {when (get #editable cardV) (renderCardUpdateAddForm card)}
                <div class="mt-4">
                    {forEach (pinnedUpdates <> otherUpdates) renderCardUpdate}
                </div>
            |]
        reverseOrderUpdates =
            [hsx|
                <p class="text-muted small">Comment order: oldest to newest.</p>
                <div class="mb-3">
                    {forEach (reverse pinnedUpdates <> reverse otherUpdates) renderCardUpdate}
                </div>
                {when (get #editable cardV) (renderCardUpdateAddForm card)}
            |]
        archivedBadge = [hsx|<span class="badge badge-secondary mr-2">Archived</span>|]

renderCardEditButton :: Card -> Html
renderCardEditButton card =
    [hsx|
        <a
          href={EditCardAction (get #id card)}
          class="btn btn-sm btn-outline-info"
          style="margin-left:1em"
        >
          Edit
        </a>
    |]

renderCardDeleteButton :: Card -> Html
renderCardDeleteButton card =
    [hsx|
        <a
          href={DeleteCardAction (get #id card)}
          class="btn btn-sm btn-outline-danger js-delete"
          style="margin-left:1em"
        >
          Delete
        </a>
    |]

renderCardUpdateAddForm :: Card -> Html
renderCardUpdateAddForm card =
    formForWithOptions
        cardUpdate
        options
        [hsx|
            <style>
              .update-content-field { max-width:40rem; width:100%; }
            </style>
            {(textareaField #content) {
               disableLabel = True,
               fieldClass = "update-content-field use-tiptap"
             }
            }
            {submitButton {
               label = "Post"
             }
            }
            <div class="ml-4 custom-control custom-control-inline custom-checkbox">
              <input type="checkbox" class="custom-control-input" id="private" name="private">
              <label class="custom-control-label" for="private">🔒 Private comment</label>
            </div>
        |]
  where
    cardUpdate = (newRecord :: CardUpdate)

    options :: FormContext CardUpdate -> FormContext CardUpdate
    options formContext =
        formContext
            |> set #formAction (pathTo (CreateCardUpdateAction (get #id card)))
            |> set #formId "woc-card-update-add-form"
            |> if card ^. #settings_ % #reverseOrder
                then set #customFormAttributes [("onsubmit", "window.__scrollTo = 'footer'")]
                else Prelude.id

renderCardUpdate ::
    CardUpdateV ->
    Html
renderCardUpdate cardUpdateV =
    [hsx|
  <div class={T.unwords [
                "woc-card-update",
                if private then "woc-card-update-private" else "",
                if pinned then "woc-card-update-pinned" else ""
              ]}
       id={"comment-" <> show (get #id cardUpdate)}>
    <div style="margin-bottom:.3em">
      <span class="text-muted small">
        <a href={pathTo (ShowCardAction cardId) <> "#comment-" <> show (get #id cardUpdate)}>
            {renderTimestamp (get #createdAt cardUpdate)}
        </a>
      </span>
      {when private lockIcon}
      <div class="ml-3 d-inline-flex">
        {when (get #editable cardUpdateV) (renderCardUpdateEditButton cardUpdate)}
        {when (get #editable cardUpdateV) (renderCardUpdateDeleteButton cardUpdate)}
        <!-- We always render the reply button because we want to nudge people to sign up -->
        {renderCardUpdateReplyButton cardUpdate}
      </div>
    </div>
    <div class="rendered-content">
      {renderMarkdown (get #content cardUpdate)}
    </div>
    <div class="woc-card-update-replies ml-5">
      {forEach (get #replies cardUpdateV) (renderReply cardUpdate)}
    </div>
  </div>
  |]
  where
    cardUpdate = get #cardUpdate cardUpdateV
    cardId = get #id (get #card cardUpdateV)
    private = case cardUpdate ^. #settings_ % #visibility of
        VisibilityPublic -> False
        VisibilityPrivate -> True
    pinned = cardUpdate ^. #settings_ % #pinned

renderCardUpdateEditButton cardUpdate =
    [hsx|
  <a class="btn btn-tiny btn-outline-info mr-1"
     href={EditCardUpdateAction (get #id cardUpdate)}>
    Edit
  </a>|]

renderCardUpdateDeleteButton cardUpdate =
    [hsx|
  <a class="btn btn-tiny btn-outline-danger js-delete mr-1"
     href={DeleteCardUpdateAction (get #id cardUpdate)}>
    Kill
  </a>|]

renderCardUpdateReplyButton cardUpdate =
    [hsx|
  <a class="btn btn-tiny btn-outline-secondary mr-1"
     href={NewReplyAction (get #id cardUpdate) (show replySource)}>
    Reply
  </a>|]
  where
    replySource = ReplySourceCard (get #cardId cardUpdate)

renderReply :: CardUpdate -> ReplyV -> Html
renderReply cardUpdate replyV =
    [hsx|
  <div id={"reply-" <> show (get #id reply)} class="reply media">
    {gravatar}
    <div class="media-body ml-1">
      <div class="woc-reply-info">
        <span class="text-muted small">
          {authorName}
          <span>
            <a href={pathTo (ShowCardAction (get #cardId replyV)) <> "#reply-" <> show (get #id reply)}>
                {renderTimestamp createdAt}
            </a>
          </span>
          <div class="ml-2 d-inline">
            {when (get #editable replyV) $ renderReplyEditButton cardUpdate reply}
            {when (get #markAsReadAble replyV) $ renderReplyMarkAsReadButton cardUpdate reply}
            {when (get #deletable replyV) $ renderReplyDeleteButton cardUpdate reply}
          </div> 
        </span>
      </div>
      <div class="woc-reply-content rendered-content small">{renderMarkdown content}</div>
    </div>
  </div>
  |]
  where
    reply@Reply{..} = get #reply replyV
    authorName = case get #author replyV of
        Nothing -> [hsx|<span class="mr-2 font-weight-bold">[deleted]</span>|]
        Just author ->
            [hsx|
        <span class="mr-2 font-weight-bold">
          <a href={ShowUserAction (get #id author)}>{get #displayName author}</a>
        </span>
      |]
    gravatar = case get #author replyV of
        Nothing -> [hsx|<span>{gravatarTiny ""}</span>|]
        Just author -> [hsx|<a href={ShowUserAction (get #id author)}>{gravatarTiny (get #email author)}</a>|]

renderReplyEditButton :: CardUpdate -> Reply -> Html
renderReplyEditButton cardUpdate reply =
    [hsx|
  <a class="btn btn-tiny btn-outline-info"
     href={EditReplyAction (get #id reply) (show replySource)}>
    Edit
  </a>|]
  where
    replySource = ReplySourceCard (get #cardId cardUpdate)

renderReplyDeleteButton :: CardUpdate -> Reply -> Html
renderReplyDeleteButton cardUpdate reply =
    [hsx|
  <a class="btn btn-tiny btn-outline-danger js-delete"
     href={DeleteReplyAction (get #id reply) (show replySource)}>
    Kill
  </a>|]
  where
    replySource = ReplySourceCard (get #cardId cardUpdate)

renderReplyMarkAsReadButton :: CardUpdate -> Reply -> Html
renderReplyMarkAsReadButton cardUpdate reply =
    [hsx|
  <form class="d-inline" method="POST" action={UpdateMarkReplyAsReadAction (get #id reply) (show replySource)}>
    <button class="btn btn-tiny btn-outline-info">Mark as read</button>
  </form>
  |]
  where
    replySource = ReplySourceCard (get #cardId cardUpdate)
