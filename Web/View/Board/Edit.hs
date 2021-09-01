module Web.View.Board.Edit where
import Web.View.Prelude

data EditView = EditView { board :: Board }

instance View EditView where
    html EditView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={BoardsAction}>Boards</a></li>
                <li class="breadcrumb-item active">Edit Board</li>
            </ol>
        </nav>
        <h1>Edit Board</h1>
        {renderForm board}
    |]

renderForm :: Board -> Html
renderForm board = formFor board [hsx|
    {(textField #title)}
    {submitButton}
|]
