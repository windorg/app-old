module Web.Controller.Sessions where

import Web.Controller.Prelude
import Web.View.Sessions.LoginOrSignup
import qualified IHP.AuthSupport.Lockable as Lockable
import qualified IHP.AuthSupport.Controller.Sessions as Sessions

instance Controller LoginController where
    action LoginOrSignupAction = do
        let alreadyLoggedIn = isJust currentUserOrNothing
        when alreadyLoggedIn (redirectToPath (Sessions.afterLoginRedirectPath @User))
        let user = newRecord @User
        render LoginOrSignupView { loginUser = user, signupUser = user }

    action CreateSessionAction =
        query @User
        |> filterWhereCaseInsensitive (#email, param "email")
        |> fetchOneOrNothing
        >>= \case
            Just (user :: record) -> do
                isLocked <- Lockable.isLocked user
                when isLocked do
                    setErrorMessage "User is locked"
                    redirectTo LoginOrSignupAction
                if verifyPassword user (param @Text "password")
                    then do
                        Sessions.beforeLogin user
                        login user
                        user <- user
                                |> set #failedLoginAttempts 0
                                |> updateRecord
                        redirectUrl <- getSessionAndClear "IHP.LoginSupport.redirectAfterLogin"
                        redirectToPath (fromMaybe (Sessions.afterLoginRedirectPath @User) redirectUrl)
                    else do
                        setErrorMessage "Invalid credentials"
                        user <- user
                                |> incrementField #failedLoginAttempts
                                |> updateRecord
                        when (get #failedLoginAttempts user >= Sessions.maxFailedLoginAttemps user) do
                            Lockable.lock user
                            pure ()
                        redirectTo LoginOrSignupAction
            Nothing -> do
                setErrorMessage "Invalid credentials"
                redirectTo LoginOrSignupAction

    action LogoutAction = do
        case currentUserOrNothing of
            Just user -> logout user
            Nothing -> pure ()
        redirectToPath "/"

instance Sessions.SessionsControllerConfig User