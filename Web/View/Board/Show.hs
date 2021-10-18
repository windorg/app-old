module Web.View.Board.Show where

import Web.View.Prelude
import Web.Helper.View
import Named

data ShowView = ShowView {owner :: User, board :: Board, cards :: [(Card, Int)]}

instance View ShowView where
  html ShowView {..} =
    [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={BoardsAction}>Boards</a></li>
                <li class="breadcrumb-item">
                  <a href={ShowUserAction (get #id owner)}>
                    <em>@{get #handle owner}</em>
                  </a>
                </li>
                <li class="breadcrumb-item active">{get #title board}</li>
            </ol>
        </nav>
        <h1 style="margin-bottom:1em;">
          {when private lockIcon}{" " :: Text}
          {get #title board}
        </h1>
        {when editable (renderCardAddForm board)}
        <div style="margin-top:30px;">
          {forEach cards renderCard}
        </div>
    |]
    where
      editable = mbCurrentUserId == Just (get #ownerId board)
      private = case board ^. #settings_ % #visibility of
          VisibilityPublic -> False
          VisibilityPrivate -> True

renderCard :: (Card, Int) -> Html
renderCard (card, count) =
  [hsx|
    <div class={"card mb-2 woc-card " <> if private then "woc-card-private" else "" :: Text}>
      <div class="card-body">
        {when private lockIcon}{" " :: Text}
        <a class="stretched-link" href={ShowCardAction (get #id card)}>{get #title card}</a>
        <span style="margin-left:.5em;" class="badge badge-secondary">{count}</span>
      </div>
    </div>
  |]
  where
    private = case card ^. #settings_ % #visibility of
      VisibilityPublic -> False
      VisibilityPrivate -> True

renderCardAddForm :: Board -> Html
renderCardAddForm board =
  formForWithOptions
    card
    options
    [hsx|
  <style>
    .title-field { max-width:40rem; width:100%; }
  </style>
  {(textField #title) {
     disableLabel = True,
     placeholder = "Card title",
     fieldClass = "title-field"
   }
  }
  {submitButton {
    label = "Add a card"
   }
  }
  <div class="ml-4 custom-control custom-control-inline custom-checkbox">
    <input type="checkbox" class="custom-control-input" id="private" name="private">
    <label class="custom-control-label" for="private">🔒 Private card</label>
  </div>
  |]
  where
    card = (newRecord :: Card)

    options :: FormContext Card -> FormContext Card
    options formContext =
      formContext
        |> set #formAction (pathTo (CreateCardAction (get #id board)))
