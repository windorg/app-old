module Web.Controller.Board where

import Web.Controller.Prelude
import Web.View.Board.Index
import Web.View.Board.New
import Web.View.Board.Edit
import Web.View.Board.Show

instance Controller BoardController where
    action BoardsAction = do
        board <- query @Board |> fetch
        render IndexView { .. }

    action NewBoardAction = do
        let board = newRecord
        render NewView { .. }

    action ShowBoardAction { boardId } = do
        board <- fetch boardId
        cards <- get #cards board |> orderByDesc #createdAt |> fetch
        counts <- forM cards $ \card -> 
            sqlQueryScalar "SELECT COUNT(*) FROM card_updates WHERE card_id = ?" [get #id card]
        render ShowView { cards = zip cards counts, .. }

    action EditBoardAction { boardId } = do
        board <- fetch boardId
        render EditView { .. }

    action UpdateBoardAction { boardId } = do
        board <- fetch boardId
        board
            |> buildBoard
            |> ifValid \case
                Left board -> render EditView { .. }
                Right board -> do
                    board <- board |> updateRecord
                    setSuccessMessage "Board updated"
                    redirectTo EditBoardAction { .. }

    action CreateBoardAction = do
        let board = newRecord @Board
        board
            |> buildBoard
            |> ifValid \case
                Left board -> render NewView { .. } 
                Right board -> do
                    board <- board |> createRecord
                    setSuccessMessage "Board created"
                    redirectTo BoardsAction

    action DeleteBoardAction { boardId } = do
        board <- fetch boardId
        deleteRecord board
        setSuccessMessage "Board deleted"
        redirectTo BoardsAction

buildBoard board = board
    |> fill @["title","userId"]
