module Web.View.Board.New where
import Web.View.Prelude

data NewView = NewView { board :: Board }

instance View NewView where
    html NewView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={BoardsAction}>Boards</a></li>
                <li class="breadcrumb-item active">New Board</li>
            </ol>
        </nav>
        <h1>New Board</h1>
        {renderForm board}
    |]

renderForm :: Board -> Html
renderForm board = formFor board [hsx|
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