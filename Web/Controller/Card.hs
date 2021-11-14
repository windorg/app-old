module Web.Controller.Card where

import Web.Controller.Prelude
import Web.View.Card.Edit
import Web.View.Card.Show
import Web.Controller.Authorization
import Named
import Control.Monad (filterM)
import Web.ViewTypes
import qualified Optics
import Data.Text (strip)

instance Controller CardController where
    action ShowCardAction { cardId } = do
        accessDeniedUnless =<< userCanView @Card cardId
        card <- fetch cardId
        board <- fetch (get #boardId card)
        owner <- fetch (get #ownerId board)
        cardUpdates <- 
          get #cardUpdates card 
            |> orderByDesc #createdAt
            |> fetch
            >>= filterM (userCanView @CardUpdate . get #id)
        replySets <- forM cardUpdates \c ->
            get #replies c
              |> orderByAsc #createdAt
              |> fetch
              >>= filterM (userCanView @Reply . get #id)
              >>= mapM fetchReplyV
        render ShowView { cardUpdates = zip cardUpdates replySets, .. }

    action EditCardAction { cardId } = do
        ensureIsUser
        accessDeniedUnless =<< userCanEdit @Card cardId
        card <- fetch cardId
        board <- fetch (get #boardId card)
        owner <- fetch (get #ownerId board)
        ownBoards <- query @Board 
            |> filterWhere (#ownerId, currentUserId)
            |> orderByDesc #createdAt
            |> fetch
        render EditView { .. }

    action UpdateCardAction { cardId } = do
        ensureIsUser
        accessDeniedUnless =<< userCanEdit @Card cardId
        card <- fetch cardId
        board <- fetch (get #boardId card)
        owner <- fetch (get #ownerId board)
        ownBoards <- query @Board 
            |> filterWhere (#ownerId, currentUserId)
            |> orderByDesc #createdAt
            |> fetch
        card
            |> buildCard
            |> fill @'["boardId"]
            |> validateField #boardId (\bid -> 
                if bid `elem` map (get #id) ownBoards
                    then Success
                    else Failure "The board must belong to you")
            |> ifValid \case
                Left card -> do
                    render EditView { .. }
                Right card -> do
                    card <- card |> updateRecord
                    redirectTo ShowCardAction { .. }

    action CreateCardAction { boardId } = do
        board <- fetch boardId
        accessDeniedUnless =<< userCanEdit @Board boardId
        let card = (newRecord :: Card)
                |> set #ownerId (get #ownerId board)
                |> set #boardId boardId
        card
            |> buildCard
            |> modify #title strip
            |> ifValid \case
                Left card -> do
                    setErrorMessage "Card is invalid"
                    redirectTo ShowBoardAction { .. }
                Right card -> do
                    card <- card |> createRecord
                    redirectTo ShowBoardAction { .. }

    action DeleteCardAction { cardId } = do
        accessDeniedUnless =<< userCanEdit @Card cardId
        card <- fetch cardId
        deleteRecord card
        redirectTo ShowBoardAction { boardId = get #boardId card }

buildCard card = card
    |> fill @'["title"]
    |> Optics.set #settings_ CardSettings{
         visibility = if paramOrDefault False "private" then VisibilityPrivate else VisibilityPublic,
         reverseOrder = paramOrDefault False "reverseOrder",
         archived = paramOrDefault False "archived"
       }
    