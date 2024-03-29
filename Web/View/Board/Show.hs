module Web.View.Board.Show where

import Named
import Optics (view)
import Web.Helper.View
import Web.View.Prelude

data ShowView = ShowView {owner :: User, board :: Board, cards :: [(Card, Int)]}

instance View ShowView where
    beforeRender ShowView{..} = do
        setTitle (get #title board <> " / wind of change")
        setOGTitle (get #title board)
        setDescription $ format "by {} @{}" (get #displayName owner) (get #handle owner)
        setOGDescription $ format "by {} @{}" (get #displayName owner) (get #handle owner)

    html ShowView{..} =
        [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={BoardsAction}>Boards</a></li>
                {userCrumb (($) #active False) owner}
                {boardCrumb (($) #active True) board}
            </ol>
        </nav>
        <h1 style="margin-bottom:1em;">
          {when private lockIcon}
          {get #title board}
        </h1>
        {when editable (renderCardAddForm board)}
        <div style="margin-top:30px;">
          {forEach normalCards renderCard}
        </div>
        {when (not (null archivedCards)) archive}
    |]
      where
        editable = mbCurrentUserId == Just (get #ownerId board)
        private = case board ^. #settings_ % #visibility of
            VisibilityPublic -> False
            VisibilityPrivate -> True
        (normalCards, archivedCards) = partition (not . view (#settings_ % #archived) . fst) cards
        archive =
            [hsx|
                <details class="mt-3">
                    <summary>
                        <span class="badge badge-secondary mb-2">Archived</span>
                    </summary>
                    {forEach archivedCards renderCard}
                </details>
            |]

renderCard :: (Card, Int) -> Html
renderCard (card, count) =
    [hsx|
    <div class={"card mb-2 woc-card " <> if private then "woc-card-private" else "" :: Text}>
      <div class="card-body">
        {when private (lockIcon <> space)}
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
