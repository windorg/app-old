module Web.View.Board.Index where
import Web.View.Prelude

data IndexView = IndexView { board :: [Board] }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item active"><a href={BoardsAction}>Boards</a></li>
            </ol>
        </nav>
        <h1>Index <a href={pathTo NewBoardAction} class="btn btn-primary ml-4">+ New</a></h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>Board</th>
                        <th></th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach board renderBoard}</tbody>
            </table>
        </div>
    |]


renderBoard :: Board -> Html
renderBoard board = [hsx|
    <tr>
        <td>{board}</td>
        <td><a href={ShowBoardAction (get #id board)}>Show</a></td>
        <td><a href={EditBoardAction (get #id board)} class="text-muted">Edit</a></td>
        <td><a href={DeleteBoardAction (get #id board)} class="js-delete text-muted">Delete</a></td>
    </tr>
|]
