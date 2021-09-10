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
|]
