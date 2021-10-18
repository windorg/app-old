module Web.View.Card.Edit where
import Web.View.Prelude

data EditView = EditView { owner :: User, board :: Board, card :: Card }

instance View EditView where
    html EditView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={BoardsAction}>Boards</a></li>
                <li class="breadcrumb-item">
                  <a href={ShowUserAction (get #id owner)}>
                    <em>@{get #handle owner}</em>
                  </a>
                </li>
                <li class="breadcrumb-item">
                  <a href={ShowBoardAction (get #id board)}>{get #title board}</a>
                </li>
                <li class="breadcrumb-item">
                  <a href={ShowCardAction (get #id card)}>{get #title card}</a>
                </li>
                <li class="breadcrumb-item active">Edit card</li>
            </ol>
        </nav>
        <h1>Edit card</h1>
        {renderForm card}
    |]

renderForm :: Card -> Html
renderForm card = formFor card [hsx|
    {(textField #title)}
    <div class="custom-control custom-checkbox">
      <input type="checkbox" class="custom-control-input" name="reverseOrder" id="reverseOrder" checked={reverseOrder}>
      <label class="custom-control-label" for="reverseOrder">
        Show comments in reverse order<br>
        <span class="text-muted small">
          Good for cards that work like blog posts. Or maybe you just really like the reverse order.
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