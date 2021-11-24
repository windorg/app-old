module Web.Controller.CardUpdate where

import qualified Optics
import Web.Controller.Authorization
import Web.Controller.Prelude
import Web.View.CardUpdate.Edit

instance Controller CardUpdateController where
    action EditCardUpdateAction{cardUpdateId} = do
        accessDeniedUnless =<< userCanEdit @CardUpdate cardUpdateId
        cardUpdate <- fetch cardUpdateId
        card <- fetch (get #cardId cardUpdate)
        board <- fetch (get #boardId card)
        owner <- fetch (get #ownerId board)
        render EditView{..}
    action UpdateCardUpdateAction{cardUpdateId} = do
        accessDeniedUnless =<< userCanEdit @CardUpdate cardUpdateId
        cardUpdate <- fetch cardUpdateId
        card <- fetch (get #cardId cardUpdate)
        board <- fetch (get #boardId card)
        owner <- fetch (get #ownerId board)
        cardUpdate
            |> buildCardUpdate
            |> ifValid \case
                Left cardUpdate -> do
                    card :: Card <- fetch (get #cardId cardUpdate)
                    render EditView{..}
                Right cardUpdate -> do
                    cardUpdate <- cardUpdate |> updateRecord
                    redirectTo ShowCardAction{cardId = get #cardId cardUpdate}
    action CreateCardUpdateAction{cardId} = do
        card <- fetch cardId
        accessDeniedUnless =<< userCanEdit @Card cardId
        let cardUpdate =
                (newRecord :: CardUpdate)
                    |> set #ownerId (get #ownerId card)
                    |> set #cardId cardId
        cardUpdate
            |> buildCardUpdate
            |> ifValid \case
                Left cardUpdate -> do
                    setErrorMessage "Card update is invalid"
                    redirectTo ShowCardAction{..}
                Right cardUpdate -> do
                    cardUpdate <- cardUpdate |> createRecord
                    redirectTo ShowCardAction{..}
    action DeleteCardUpdateAction{cardUpdateId} = do
        accessDeniedUnless =<< userCanEdit @CardUpdate cardUpdateId
        cardUpdate <- fetch cardUpdateId
        deleteRecord cardUpdate
        redirectTo ShowCardAction{cardId = get #cardId cardUpdate}

buildCardUpdate cardUpdate =
    cardUpdate
        |> fill @'["content"]
        |> Optics.set
            #settings_
            CardUpdateSettings
                { visibility = if paramOrDefault False "private" then VisibilityPrivate else VisibilityPublic,
                  subscribers = mempty
                }
