module Web.View.Board.Show where
import Web.View.Prelude

data ShowView = ShowView { board :: Board }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={BoardsAction}>Boards</a></li>
                <li class="breadcrumb-item active">Show Board</li>
            </ol>
        </nav>
        <h1>Show Board</h1>
        <p>{board}</p>
    |]
