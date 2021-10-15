module Web.View.Board.Index where

import Web.View.Prelude
import Web.Helper.View
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
