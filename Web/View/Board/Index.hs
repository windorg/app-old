module Web.View.Board.Index where
import Web.View.Prelude

data IndexView = IndexView { board :: [Board] }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item active"><a href={BoardsAction}>My boards</a></li>
            </ol>
        </nav>
        <a href={pathTo NewBoardAction} class="btn btn-primary">New board</a>
        <div class="row-cols-1 row-cols-md2">
            {forEach board renderBoard}
        </div>
    |]


renderBoard :: Board -> Html
renderBoard board = [hsx|
    <div class="card mt-3 mb-3">
        <div class="card-body">
            <h3>
                <a href={ShowBoardAction (get #id board)}>{get #title board}</a>
                {renderBoardEditButton board}
                {renderBoardDeleteButton board}
            </h3>
        </div>
    </div>
|]

renderBoardEditButton board = [hsx|
  <a
    class="btn btn-sm btn-outline-info"
    style="margin-left:.5rem; padding:.125rem .25rem; font-size:.5rem;"
    href={EditBoardAction (get #id board)}
  >
    Edit
  </a>
  |]

renderBoardDeleteButton board = [hsx|
  <a
    class="btn btn-sm btn-outline-danger js-delete"
    style="padding:.125rem .25rem; font-size:.5rem;"
    href={DeleteBoardAction (get #id board)}
  >
    Delete
  </a>
  |]