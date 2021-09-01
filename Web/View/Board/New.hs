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
|]
