module Web.Controller.Card where

import Web.Controller.Prelude
import Web.View.Card.Edit
import Web.View.Card.Show
import Web.Controller.Authorization
import Named
import Web.ViewTypes

instance Controller CardController where
    action ShowCardAction { cardId } = do
        accessDeniedUnless =<< userCanView @Card cardId
        card <- fetch cardId
        board <- fetch (get #boardId card)
        cardUpdates <- get #cardUpdates card |> orderByDesc #createdAt |> fetch
        replySets <- forM cardUpdates \c ->
            mapM fetchReplyV =<< fetch (get #replies c)
        render ShowView { cardUpdates = zip cardUpdates replySets, .. }

    action EditCardAction { cardId } = do
        accessDeniedUnless =<< userCanEdit @Card cardId
        card <- fetch cardId
        board <- fetch (get #boardId card)
        render EditView { .. }

    action UpdateCardAction { cardId } = do
        accessDeniedUnless =<< userCanEdit @Card cardId
        card <- fetch cardId
        card
            |> buildCard
            |> ifValid \case
                Left card -> render EditView { .. }
                Right card -> do
                    card <- card |> updateRecord
                    redirectTo ShowCardAction { .. }

    action CreateCardAction { boardId } = do
        accessDeniedUnless =<< userCanEdit @Board boardId
        let card = (newRecord :: Card) |> set #boardId boardId
        card
            |> fill @'["title"]
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
