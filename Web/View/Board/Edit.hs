module Web.View.Board.Edit where

import Web.Helper.View
import Web.View.Prelude

data EditView = EditView {owner :: User, board :: Board}

instance View EditView where
    beforeRender EditView{..} = do
        setTitle (get #title board <> " / wind of change")

    html EditView{..} =
        [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={BoardsAction}>Boards</a></li>
                {userCrumb (($) #active False) owner}
                {boardCrumb (($) #active False) board}
                <li class="breadcrumb-item active">Edit</li>
            </ol>
        </nav>
        <h1>Edit board</h1>
        {renderForm board}
    |]

renderForm :: Board -> Html
renderForm board =
    formFor
        board
        [hsx|
    {(textField #title)}
    {submitButton}
    <div class="ml-4 custom-control custom-control-inline custom-checkbox">
      <input type="checkbox" class="custom-control-input" name="private" id="private" checked={private}>
      <label class="custom-control-label" for="private">🔒 Private board</label>
    </div>
|]
  where
    private = case board ^. #settings_ % #visibility of
        VisibilityPrivate -> True
        VisibilityPublic -> False
