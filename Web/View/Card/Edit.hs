module Web.View.Card.Edit where
import Web.View.Prelude

data EditView = EditView { board :: Board, card :: Card }

instance View EditView where
    html EditView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={BoardsAction}>Boards</a></li>
                <li class="breadcrumb-item">
                  <a href={ShowBoardAction (get #id board)}>{get #title board}</a>
                </li>
                <li class="breadcrumb-item">
                  <a href={ShowCardAction (get #id card)}>{get #title card}</a>
                </li>
                <li class="breadcrumb-item active">Edit card</li>
            </ol>
        </nav>
        <h1>Edit Card</h1>
        {renderForm card}
    |]

renderForm :: Card -> Html
renderForm card = formFor card [hsx|
    {(textField #title)}
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