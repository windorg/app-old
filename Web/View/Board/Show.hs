module Web.View.Board.Show where

import Web.View.Prelude
import Named

data ShowView = ShowView {board :: Board, cards :: [(Card, Int)]}

instance View ShowView where
  html ShowView {..} =
    [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={BoardsAction}>Boards</a></li>
                <li class="breadcrumb-item active">{get #title board}</li>
            </ol>
        </nav>
        <h1 style="margin-bottom:1em;">{get #title board}</h1>
        <div class="alert alert-warning">
          By default, boards and cards are <strong>public</strong>.
        </div>
        {when editable (renderCardAddForm board)}
        <div style="margin-top:30px;">
          {forEach cards renderCard}
        </div>
    |]
    where
      editable = (get #id <$> currentUserOrNothing) == Just (get #userId board)

renderCard :: (Card, Int) -> Html
renderCard (card, count) =
  [hsx|
    <div class="card mb-2">
      <div class="card-body">
        <a class="stretched-link" href={ShowCardAction (get #id card)}>{get #title card}</a>
        <span style="margin-left:.5em;" class="badge badge-secondary">{count}</span>
      </div>
    </div>
  |]

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
  |]
  where
    card = (newRecord :: Card)

    options :: FormContext Card -> FormContext Card
    options formContext =
      formContext
        |> set #formAction (pathTo (CreateCardAction (get #id board)))
