module Web.View.Card.Show where

import Web.View.Prelude
import Named
import Web.Controller.Authorization
import Web.ViewTypes
import Web.Helper.View
import qualified Prelude
import qualified Optics

data ShowView = ShowView {
    owner :: User,
    board :: Board,
    card :: Card,
    cardUpdates :: [(CardUpdate, [ReplyV])]
    }

instance View ShowView where
    beforeRender ShowView{..} = do
      setTitle (get #title card <> " / wind of change")

    html ShowView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={BoardsAction}>Boards</a></li>
                <li class="breadcrumb-item">
                  <a href={ShowUserAction (get #id owner)}>
                    <em>@{get #handle owner}</em>
                  </a>
                </li>
                <li class="breadcrumb-item">
                  <a href={ShowBoardAction (get #id board)}>{get #title board}</a>
                </li>
                <li class="breadcrumb-item active">{get #title card}</li>
            </ol>
        </nav>
        <h1 class="mb-4">
          {when private lockIcon}
          {get #title card}
          {when editable (renderCardEditButton card <> renderCardDeleteButton card)}
        </h1>
        {if reverseOrder then reverseOrderHtml else normalOrderHtml}
     |]
     where
       editable = mbCurrentUserId == Just (get #ownerId board)
       private = case card ^. #settings_ % #visibility of
         VisibilityPublic -> False
         VisibilityPrivate -> True
       reverseOrder = card ^. #settings_ % #reverseOrder
       normalOrderHtml = [hsx|
         {when editable (renderCardUpdateAddForm card)}
         <div class="mt-4">
           {forEach cardUpdates (renderCardUpdate (($) #editable editable) card)}
         </div>
       |]
       reverseOrderHtml = [hsx|
         <p class="text-muted small">Comment order: oldest to newest.</p>
         <div class="mb-3">
           {forEach (reverse cardUpdates) (renderCardUpdate (($) #editable editable) card)}
         </div>
         {when editable (renderCardUpdateAddForm card)}
       |]

renderCardEditButton :: Card -> Html
renderCardEditButton card = [hsx|
  <a
    href={EditCardAction (get #id card)}
    class="btn btn-sm btn-outline-info"
    style="margin-left:1em"
  >
    Edit
  </a>
  |]

renderCardDeleteButton :: Card -> Html
renderCardDeleteButton card = [hsx|
  <a
    href={DeleteCardAction (get #id card)}
    class="btn btn-sm btn-outline-danger js-delete"
    style="margin-left:1em"
  >
    Delete
  </a>
  |]

renderCardUpdateAddForm :: Card -> Html
renderCardUpdateAddForm card = formForWithOptions cardUpdate options [hsx|
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
    options formContext = formContext
      |> set #formAction (pathTo (CreateCardUpdateAction (get #id card)))
      |> set #formId "woc-card-update-add-form"
      |> if card ^. #settings_ % #reverseOrder 
           then set #customFormAttributes [("onsubmit", "window.__scrollTo = 'footer'")]
           else Prelude.id

renderCardUpdate 
  :: "editable" :! Bool
  -> Card
  -> (CardUpdate, [ReplyV]) 
  -> Html
renderCardUpdate (Arg editable) card (cardUpdate, replies) = [hsx|
  <div class={"woc-card-update " <> if private then "woc-card-update-private" else "" :: Text}>
    <div style="margin-bottom:.3em">
      <span class="text-muted small">
        {renderTimestamp (get #createdAt cardUpdate)}
      </span>
      {when private lockIcon}
      <div class="ml-3 d-inline">
        {when editable (renderCardUpdateEditButton cardUpdate)}
        {when editable (renderCardUpdateDeleteButton cardUpdate)}
        {when (isJust currentUserOrNothing) (renderCardUpdateReplyButton cardUpdate)}
      </div>
    </div>
    <div class="rendered-content">
      {renderMarkdown (get #content cardUpdate)}
    </div>
    <div class="replies ml-5">
      {forEach replies (renderReply cardUpdate)}
    </div>
  </div>
  |]
  where
    private = case cardUpdate ^. #settings_ % #visibility of
      VisibilityPublic -> False
      VisibilityPrivate -> True

renderCardUpdateEditButton cardUpdate = [hsx|
  <a class="btn btn-tiny btn-outline-info"
     href={EditCardUpdateAction (get #id cardUpdate)}>
    Edit
  </a>|]

renderCardUpdateDeleteButton cardUpdate = [hsx|
  <a class="btn btn-tiny btn-outline-danger js-delete"
     href={DeleteCardUpdateAction (get #id cardUpdate)}>
    Kill
  </a>|]

renderCardUpdateReplyButton cardUpdate = [hsx|
  <a class="btn btn-tiny btn-outline-secondary"
     href={NewReplyAction (get #id cardUpdate) (show replySource)}>
    Reply
  </a>|]
  where
    replySource = ReplySourceCard (get #cardId cardUpdate)

renderReply :: CardUpdate -> ReplyV -> Html
renderReply cardUpdate replyV = [hsx|
<div id={"reply-" <> show (get #id reply)} class="reply">
  <div class="mb-1">
    <span class="text-muted small">
      {authorName}
      <span>{renderTimestamp createdAt}</span>
      <div class="ml-2 d-inline">
        {when (get #editable replyV) $ renderReplyEditButton cardUpdate reply}
        {when (get #markAsReadAble replyV) $ renderReplyMarkAsReadButton cardUpdate reply}
        {when (get #deletable replyV) $ renderReplyDeleteButton cardUpdate reply}
      </div> 
    </span>
  </div>
  <div class="rendered-content small">{renderMarkdown content}</div>
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

renderReplyEditButton :: CardUpdate -> Reply -> Html
renderReplyEditButton cardUpdate reply = [hsx|
  <a class="btn btn-tiny btn-outline-info"
     href={EditReplyAction (get #id reply) (show replySource)}>
    Edit
  </a>|]
  where
    replySource = ReplySourceCard (get #cardId cardUpdate)

renderReplyDeleteButton :: CardUpdate -> Reply -> Html
renderReplyDeleteButton cardUpdate reply = [hsx|
  <a class="btn btn-tiny btn-outline-danger js-delete"
     href={DeleteReplyAction (get #id reply) (show replySource)}>
    Kill
  </a>|]
  where
    replySource = ReplySourceCard (get #cardId cardUpdate)

renderReplyMarkAsReadButton :: CardUpdate -> Reply -> Html
renderReplyMarkAsReadButton cardUpdate reply = [hsx|
  <form class="d-inline" method="POST" action={UpdateMarkReplyAsReadAction (get #id reply) (show replySource)}>
    <button class="btn btn-tiny btn-outline-info">Mark as read</button>
  </form>
  |]
  where
    replySource = ReplySourceCard (get #cardId cardUpdate)