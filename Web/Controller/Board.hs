module Web.Controller.Board where

import qualified Optics
import Web.Controller.Authorization
import Web.Controller.Prelude
import Web.Helper.Common
import Web.View.Board.Edit
import Web.View.Board.Index
import Web.View.Board.New
import Web.View.Board.Show

import Data.Text (strip)
import Named

instance Controller BoardController where
    action BoardsAction = do
        let augmentBoard board = do
                user <- fetch (get #ownerId board)
                pure (board, #handle (get #handle user), #displayName (get #displayName user))
        case mbCurrentUserId of
            Just currentUid -> do
                ownBoards <-
                    query @Board
                        |> filterWhere (#ownerId, currentUid)
                        |> orderByDesc #createdAt
                        |> fetch
                othersBoards <-
                    query @Board
                        |> filterWhereNot (#ownerId, currentUid)
                        |> orderByDesc #createdAt
                        |> fetch
                        >>= filterM (userCanView @Board . get #id)
                        >>= mapM augmentBoard
                render IndexViewUser{..}
            Nothing -> do
                allBoards <-
                    query @Board
                        |> orderByDesc #createdAt
                        |> fetch
                        >>= filterM (userCanView @Board . get #id)
                        >>= mapM augmentBoard
                render IndexViewGuest{..}
    action NewBoardAction = do
        ensureIsUser
        let board =
                (newRecord :: Board)
                    |> Optics.set
                        #settings_
                        BoardSettings
                            { visibility = VisibilityPublic
                            }
        render NewView{..}
    action ShowBoardAction{boardId} = do
        accessDeniedUnless =<< userCanView @Board boardId
        board <- fetch boardId
        owner <- fetch (get #ownerId board)
        cards <-
            get #cards board
                |> orderByDesc #createdAt
                |> fetch
                >>= filterM (userCanView @Card . get #id)
        counts <- forM cards $ \card ->
            sqlQueryScalar "SELECT COUNT(*) FROM card_updates WHERE card_id = ?" [get #id card]
        render ShowView{cards = zip cards counts, ..}
    action EditBoardAction{boardId} = do
        accessDeniedUnless =<< userCanEdit @Board boardId
        board <- fetch boardId
        owner <- fetch (get #ownerId board)
        render EditView{..}
    action UpdateBoardAction{boardId} = do
        accessDeniedUnless =<< userCanEdit @Board boardId
        board <- fetch boardId
        owner <- fetch (get #ownerId board)
        board
            |> buildBoard
            |> ifValid \case
                Left board -> render EditView{..}
                Right board -> do
                    board <- board |> updateRecord
                    redirectTo BoardsAction
    action CreateBoardAction = do
        ensureIsUser
        let board = (newRecord :: Board) |> set #ownerId currentUserId
        board
            |> buildBoard
            |> modify #title strip
            |> ifValid \case
                Left board -> render NewView{..}
                Right board -> do
                    board <- board |> createRecord
                    redirectTo BoardsAction
    action DeleteBoardAction{boardId} = do
        accessDeniedUnless =<< userCanEdit @Board boardId
        board <- fetch boardId
        deleteRecord board
        redirectTo BoardsAction

buildBoard board =
    board
        |> fill @'["title"]
        |> Optics.set
            #settings_
            BoardSettings
                { visibility = if paramOrDefault False "private" then VisibilityPrivate else VisibilityPublic
                }
