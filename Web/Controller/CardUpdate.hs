module Web.Controller.CardUpdate where

import Web.Controller.Prelude
import Web.View.CardUpdate.Edit
import Web.View.CardUpdate.Show
import Web.Controller.Authorization

instance Controller CardUpdateController where
    action ShowCardUpdateAction { cardUpdateId } = do
        accessDeniedUnless =<< userCanView @CardUpdate cardUpdateId
        cardUpdate <- fetch cardUpdateId
        render ShowView { .. }

    action EditCardUpdateAction { cardUpdateId } = do
        accessDeniedUnless =<< userCanEdit @CardUpdate cardUpdateId
        cardUpdate <- fetch cardUpdateId
        card :: Card <- fetch (get #cardId cardUpdate)
        render EditView { .. }

    action UpdateCardUpdateAction { cardUpdateId } = do
        accessDeniedUnless =<< userCanEdit @CardUpdate cardUpdateId
        cardUpdate <- fetch cardUpdateId
        cardUpdate
            |> buildCardUpdate
            |> ifValid \case
                Left cardUpdate -> do
                    card :: Card <- fetch (get #cardId cardUpdate)
                    render EditView { .. }
                Right cardUpdate -> do
                    cardUpdate <- cardUpdate |> updateRecord
                    redirectTo ShowCardAction { cardId = get #cardId cardUpdate }

    action CreateCardUpdateAction { cardId } = do
        accessDeniedUnless =<< userCanEdit @Card cardId
        let cardUpdate = (newRecord :: CardUpdate) {
                cardId = cardId
            }
        cardUpdate
            |> fill @'["content"]
            |> ifValid \case
                Left cardUpdate -> do
                    setErrorMessage "Card update is invalid"
                    redirectTo ShowCardAction { .. }
                Right cardUpdate -> do
                    cardUpdate <- cardUpdate |> createRecord
                    redirectTo ShowCardAction { .. }

    action DeleteCardUpdateAction { cardUpdateId } = do
        accessDeniedUnless =<< userCanEdit @CardUpdate cardUpdateId
        cardUpdate <- fetch cardUpdateId
        deleteRecord cardUpdate
        redirectTo ShowCardAction { cardId = get #cardId cardUpdate }

buildCardUpdate cardUpdate = cardUpdate
    |> fill @["content","cardId"]
