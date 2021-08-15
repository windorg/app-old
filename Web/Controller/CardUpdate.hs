module Web.Controller.CardUpdate where

import Web.Controller.Prelude
import Web.View.CardUpdate.Index
import Web.View.CardUpdate.New
import Web.View.CardUpdate.Edit
import Web.View.CardUpdate.Show

instance Controller CardUpdateController where
    action CardUpdatesAction = do
        cardUpdate <- query @CardUpdate |> fetch
        render IndexView { .. }

    action NewCardUpdateAction = do
        let cardUpdate = newRecord
        render NewView { .. }

    action ShowCardUpdateAction { cardUpdateId } = do
        cardUpdate <- fetch cardUpdateId
        render ShowView { .. }

    action EditCardUpdateAction { cardUpdateId } = do
        cardUpdate <- fetch cardUpdateId
        card :: Card <- fetch (get #cardId cardUpdate)
        render EditView { .. }

    action UpdateCardUpdateAction { cardUpdateId } = do
        cardUpdate <- fetch cardUpdateId
        cardUpdate
            |> buildCardUpdate
            |> ifValid \case
                Left cardUpdate -> do
                    card :: Card <- fetch (get #cardId cardUpdate)
                    render EditView { .. }
                Right cardUpdate -> do
                    cardUpdate <- cardUpdate |> updateRecord
                    setSuccessMessage "CardUpdate updated"
                    redirectTo EditCardUpdateAction { .. }

    action CreateCardUpdateAction { cardId } = do
        let cardUpdate = (newRecord :: CardUpdate) {
                cardId = cardId
            }
        cardUpdate
            |> fill @'["content"]
            |> ifValid \case
                Left cardUpdate -> render NewView { .. } 
                Right cardUpdate -> do
                    cardUpdate <- cardUpdate |> createRecord
                    setSuccessMessage "Card update posted"
                    redirectTo CardUpdatesAction

    action DeleteCardUpdateAction { cardUpdateId } = do
        cardUpdate <- fetch cardUpdateId
        deleteRecord cardUpdate
        setSuccessMessage "CardUpdate deleted"
        redirectTo CardUpdatesAction

buildCardUpdate cardUpdate = cardUpdate
    |> fill @["content","cardId"]
