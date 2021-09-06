module Web.View.Board.Index where
import Web.View.Prelude
import Named

data IndexView 
    = IndexViewUser { 
        ownBoards :: [Board], 
        othersBoards :: [(Board, "handle" :! Text, "displayName" :! Text)] 
    }
    | IndexViewGuest {
        allBoards :: [(Board, "handle" :! Text, "displayName" :! Text)]
    }

instance View IndexView where
    html view = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item active"><a href={BoardsAction}>Boards</a></li>
            </ol>
        </nav>
        {render view}
    |]
        where
            render IndexViewUser{..} = [hsx|
                <h1>
                    Your boards
                    <a href={pathTo NewBoardAction} class="ml-3 btn btn-outline-primary btn-sm">+ New</a>
                </h1>
                <div class="row-cols-1 row-cols-md2">{forEach ownBoards renderOwnBoard}</div>
                <h1 class="mt-5">Others' public boards</h1>
                <div class="row-cols-1 row-cols-md2">{forEach othersBoards renderOthersBoard}</div>
            |]
            render IndexViewGuest{..} = [hsx|
                <p>To create your own boards, please <a href={NewUserAction}>sign up</a>.</p>
                <h1 class="mt-5">Public boards</h1>
                <div class="row-cols-1 row-cols-md2">{forEach allBoards renderOthersBoard}</div>
            |]


renderOwnBoard :: Board -> Html
renderOwnBoard board = [hsx|
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

renderOthersBoard :: (Board, "handle" :! Text, "displayName" :! Text) -> Html
renderOthersBoard (board, Arg handle, Arg displayName) = [hsx|
    <div class="card mt-3 mb-3">
        <div class="card-body">
            <h3>
                <a class="text-muted" href={ShowBoardAction (get #id board)}>{get #title board}</a>
            </h3>
            <span class="text-muted">
                <span class="mr-2">{displayName}</span>
                <em>@{handle}</em>
            </span>
        </div>
    </div>
|]