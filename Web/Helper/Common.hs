module Web.Helper.Common where

import Web.Controller.Prelude

getCardUpdateOwner :: (?modelContext::ModelContext) => Id CardUpdate -> IO (Id User)
getCardUpdateOwner cardUpdateId = do
    [Only owner] <- 
      sqlQuery
        [sql|
            select user_id from boards where id =
            (select board_id from cards where id =
                (select card_id from card_updates where id = ?))
        |]
        (Only cardUpdateId)
    pure owner