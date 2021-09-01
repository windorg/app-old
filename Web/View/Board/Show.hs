module Web.View.Board.Show where

import Web.View.Prelude

data ShowView = ShowView {board :: Board, cards :: [(Card, Int)]}

instance View ShowView where
  html ShowView {..} =
    [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={BoardsAction}>My boards</a></li>
                <li class="breadcrumb-item active">{get #title board}</li>
            </ol>
        </nav>
        <h1 style="margin-bottom:1em;">{get #title board}</h1>
        {if get #id currentUser == get #userId board 
           then renderCardAddForm board
           else mempty}
        <div style="margin-top:30px;">
          {forEach cards renderCard}
        </div>
    |]

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
    .title-field { max-width:500px; width:100%; }
  </style>
  {(textareaField #title) {
     placeholder = "I want to ...",
     disableLabel = True,
     fieldClass = "title-field"
   } |> autosize 5
  }
  {submitButton {
    label = "Add"
   }
  }
  |]
  where
    card = (newRecord :: Card)

    options :: FormContext Card -> FormContext Card
    options formContext =
      formContext
        |> set #formAction (pathTo (CreateCardAction (get #id board)))
