module Web.View.Card.Show where
import Web.View.Prelude
import Fmt
import qualified Commonmark

data ShowView = ShowView {
    board :: Board,
    card :: Card,
    cardUpdates :: [CardUpdate]
    }

-- Currently both "show" and "edit", kind of. Assuming the board's owner is
-- viewing the board
instance View ShowView where
    html ShowView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item">
                  <a href={ShowBoardAction (get #id board)}>{get #title board}</a>
                </li>
                <li class="breadcrumb-item active">{get #title card}</li>
            </ol>
        </nav>
        <h1 style="margin-bottom:1em">
          {get #title card}
          {renderCardDeleteButton card}
        </h1>
        {renderCardUpdateAddForm card}
        <div style="margin-top:30px;">
          {forEach cardUpdates renderCardUpdate}
        </div>
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
    .update-content-field { max-width:500px; width:100%; }
  </style>
  {(textareaField #content) {
     placeholder = "Did helpful thing X / did nothing today / etc",
     disableLabel = True,
     fieldClass = "update-content-field"
   } |> autosize 5
  }
  {submitButton {
     label = "Send"
   }
  }
  |]
  where
    cardUpdate = (newRecord :: CardUpdate)

    options :: FormContext CardUpdate -> FormContext CardUpdate
    options formContext = formContext
      |> set #formAction (pathTo (CreateCardUpdateAction (get #id card)))

-- TODO render year as well
renderTimestamp :: _ -> Text
renderTimestamp time =
    -- February 14th, 18:20
    format "{} {}, {}"
      (timeF "%B" time)
      (dayOfMonthOrdF time)
      (timeF "%R" time)

renderCardUpdate :: CardUpdate -> Html
renderCardUpdate cardUpdate = [hsx|
  <div style="margin-bottom:2em;">
    <div style="margin-bottom:.3em">
      <span class="text-muted small">
        {renderTimestamp (get #createdAt cardUpdate)}
      </span>
      {renderCardUpdateEditButton cardUpdate}
      {renderCardUpdateDeleteButton cardUpdate}
    </div>
    {renderMarkdown (get #content cardUpdate)}
  </div>
  |]

renderCardUpdateEditButton cardUpdate = [hsx|
  <a
    class="btn btn-sm btn-outline-info"
    style="margin-left:.5rem; padding:.125rem .25rem; font-size:.5rem; opacity:50%;"
    href={EditCardUpdateAction (get #id cardUpdate)}
  >
    Edit
  </a>
  |]

renderCardUpdateDeleteButton cardUpdate = [hsx|
  <a
    class="btn btn-sm btn-outline-danger js-delete js-delete-no-confirm"
    style="padding:.125rem .25rem; font-size:.5rem; opacity:50%;"
    href={DeleteCardUpdateAction (get #id cardUpdate)}
  >
    Kill
  </a>
  |]

renderMarkdown :: Text -> Html
renderMarkdown text =
    case Commonmark.commonmark "" text of
        Left err -> toHtml text
        Right (val :: Commonmark.Html ()) ->
            preEscapedToHtml (Commonmark.renderHtml val)
