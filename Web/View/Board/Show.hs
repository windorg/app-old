module Web.View.Board.Show where

import Web.View.Prelude

data ShowView = ShowView {board :: Board, cards :: [Card]}

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
        {renderCardAddForm board}
        <div style="margin-top:30px;">
          {forEach cards renderCard}
        </div>
    |]
    where
      renderCard card =
        [hsx|
          <p>
            <a href={ShowCardAction (get #id card)}>{get #title card}</a>
          </p>
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
