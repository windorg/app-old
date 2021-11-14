module Web.View.Card.Edit where

import Web.View.Prelude
import Web.Helper.View
import Named

data EditView = EditView { 
  owner :: User,
  ownBoards :: [Board],
  board :: Board,
  card :: Card 
  }

instance View EditView where
    beforeRender EditView{..} = do
      setTitle (get #title card <> " / wind of change")

    html EditView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={BoardsAction}>Boards</a></li>
                {userCrumb (($) #active False) owner}
                {boardCrumb (($) #active False) board}
                {cardCrumb (($) #active False) card}
                <li class="breadcrumb-item active">Edit</li>
            </ol>
        </nav>
        <h1>Edit card</h1>
        {renderForm (($) #ownBoards ownBoards) card}
    |]

renderForm :: "ownBoards" :! [Board] -> Card -> Html
renderForm (Arg ownBoards) card = formFor card [hsx|
    {(textField #title)}
    {(selectField #boardId ownBoards) {
      fieldLabel = "Board"
    }}
    <div class="custom-control custom-checkbox mb-3">
      <input type="checkbox" class="custom-control-input" name="reverseOrder" id="reverseOrder" checked={reverseOrder}>
      <label class="custom-control-label" for="reverseOrder">
        Show comments in reverse order<br>
        <span class="text-muted small">
          Good for cards that work like blog posts. Or maybe you just really like the reverse order.
        </span>
      </label>
    </div>
    <div class="custom-control custom-checkbox">
      <input type="checkbox" class="custom-control-input" name="archived" id="archived" checked={archived}>
      <label class="custom-control-label" for="archived">
        Archive the card<br>
        <span class="text-muted small">
          Note that the card will still be visible (unless it's private). You can unarchive it later.
        </span>
      </label>
    </div>
    <div class="mb-4"></div>
    {submitButton}
    <div class="ml-4 custom-control custom-control-inline custom-checkbox">
      <input type="checkbox" class="custom-control-input" name="private" id="private" checked={private}>
      <label class="custom-control-label" for="private">🔒 Private card</label>
    </div>
|]
  where
    private = case card ^. #settings_ % #visibility of
      VisibilityPrivate -> True
      VisibilityPublic -> False
    reverseOrder = card ^. #settings_ % #reverseOrder
    archived = card ^. #settings_ % #archived