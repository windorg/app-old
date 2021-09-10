module Web.Controller.Board where

import Web.Controller.Prelude
import Web.View.Board.Index
import Web.View.Board.New
import Web.View.Board.Edit
import Web.View.Board.Show
import Web.Controller.Authorization

import Named

instance Controller BoardController where
    action BoardsAction = do
        let augmentBoard board = do
                user <- fetch (get #userId board)
                pure (board, #handle (get #handle user), #displayName (get #displayName user))
        case currentUserOrNothing of
            Just _ -> do
                ownBoards <- query @Board 
                    |> filterWhere (#userId, currentUserId)
                    |> fetch
                othersBoards <- query @Board 
                    |> filterWhereNot (#userId, currentUserId)
                    |> fetch
                    >>= mapM augmentBoard
                render IndexViewUser { .. }
            Nothing -> do
                allBoards <- query @Board |> fetch >>= mapM augmentBoard
                render IndexViewGuest{..}

    action NewBoardAction = do
        ensureIsUser
        let board = newRecord
        render NewView { .. }

    action ShowBoardAction { boardId } = do
        accessDeniedUnless =<< userCanView @Board boardId
        board <- fetch boardId
        cards <- get #cards board |> orderByDesc #createdAt |> fetch
        counts <- forM cards $ \card -> 
            sqlQueryScalar "SELECT COUNT(*) FROM card_updates WHERE card_id = ?" [get #id card]
        render ShowView { cards = zip cards counts, .. }

    action EditBoardAction { boardId } = do
        accessDeniedUnless =<< userCanEdit @Board boardId
        board <- fetch boardId
        render EditView { .. }

    action UpdateBoardAction { boardId } = do
        accessDeniedUnless =<< userCanEdit @Board boardId
        board <- fetch boardId
        board
            |> buildBoard
            |> ifValid \case
                Left board -> render EditView { .. }
                Right board -> do
                    board <- board |> updateRecord
                    redirectTo BoardsAction

    action CreateBoardAction = do
        ensureIsUser
        let board = (newRecord :: Board) |> set #userId currentUserId
        board
            |> buildBoard
            |> ifValid \case
                Left board -> render NewView { .. } 
                Right board -> do
                    board <- board |> createRecord
                    redirectTo BoardsAction

    action DeleteBoardAction { boardId } = do
        accessDeniedUnless =<< userCanEdit @Board boardId
        board <- fetch boardId
        deleteRecord board
        redirectTo BoardsAction

buildBoard board = board
    |> fill @'["title"]
