module Web.Controller.User where

import Web.Controller.Prelude
import Web.Helper.Common
import Web.View.User.New
import Web.View.User.Edit
import Web.View.User.Show
import IHP.ValidationSupport.ValidateField (matchesRegex)
import Web.Controller.Authorization
import Data.Text (strip)

instance Controller UserController where
    action NewUserAction = do
        let user = newRecord
        render NewView { .. }

    action ShowUserAction { userId } = do
        user <- fetch userId
        boards <- query @Board 
            |> filterWhere (#ownerId, userId)
            |> orderByDesc #createdAt
            |> fetch
            >>= filterM (userCanView @Board . get #id)
        followed <- case mbCurrentUserId of 
            Nothing -> pure Nothing
            Just currentUid -> query @FollowedUser
                |> filterWhere (#subscriberId, currentUid)
                |> filterWhere (#followedUserId, userId)
                |> fetchExists
                <&> Just
        render ShowView { .. }

    action EditUserAction { userId } = do
        user <- fetch userId
        render EditView { .. }

    action UpdateUserAction { userId } = do
        user <- fetch userId
        user
            |> buildUser
            |> ifValid \case
                Left user -> render EditView { .. }
                Right user -> do
                    user <- user |> updateRecord
                    redirectTo EditUserAction { .. }

    action CreateUserAction = do
        let user = newRecord @User
        user
            |> buildUser
            |> modify #displayName strip
            |> validateField #email isEmail
            |> validateField #passwordHash nonEmpty
            |> validateField #handle (matchesRegex "^[a-zA-Z0-9_-]{1,64}$")
            |> (\u -> if get #displayName u == "" then u |> set #displayName (get #handle u) else u)
            |> ifValid \case
                Left user -> render NewView { .. } 
                Right user -> do
                    hashed <- hashPassword (get #passwordHash user)
                    user <- user
                        |> set #passwordHash hashed
                        |> createRecord
                    redirectToPath "/"

    action DeleteUserAction { userId } = do
        user <- fetch userId
        deleteRecord user
        redirectToPath "/"

    action UpdateFollowUserAction {userId} = do
        ensureIsUser
        follows <- query @FollowedUser
            |> filterWhere (#subscriberId, currentUserId)
            |> filterWhere (#followedUserId, userId)
            |> fetchExists
        unless follows do
            (newRecord :: FollowedUser)
                |> set #subscriberId currentUserId
                |> set #followedUserId userId
                |> createRecord
            pure ()
        redirectBack

    action UpdateUnfollowUserAction {userId} = do
        ensureIsUser
        mbFollow <- query @FollowedUser
            |> filterWhere (#subscriberId, currentUserId)
            |> filterWhere (#followedUserId, userId)
            |> fetchOneOrNothing
        mapM_ deleteRecord mbFollow
        redirectBack

buildUser user = user
    |> fill @["email","handle","displayName","passwordHash"]
