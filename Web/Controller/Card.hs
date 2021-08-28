module Web.Controller.Card where

import Web.Controller.Prelude
import Web.View.Card.Edit
import Web.View.Card.Show
import Web.Controller.Authorization

instance Controller CardController where
    action ShowCardAction { cardId } = do
        accessDeniedUnless =<< userCanView @Card cardId
        card <- fetch cardId
        board <- fetch (get #boardId card)
        cardUpdates <- get #cardUpdates card |> orderByDesc #createdAt |> fetch
        render ShowView { .. }

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
                    setSuccessMessage "Card updated"
                    redirectTo EditCardAction { .. }

    action CreateCardAction { boardId } = do
        accessDeniedUnless =<< userCanEdit @Board boardId
        let card = (newRecord :: Card) { boardId = boardId }
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
    |> fill @["title", "boardId"]
