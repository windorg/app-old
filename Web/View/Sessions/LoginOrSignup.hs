module Web.View.Sessions.LoginOrSignup where

import Web.View.Prelude

data LoginOrSignupView user = LoginOrSignupView
    { loginUser :: user,
      signupUser :: user
    }
    deriving (Typeable)

instance View (LoginOrSignupView User) where
    html LoginOrSignupView{..} =
        [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={BoardsAction}>Boards</a></li>
                <li class="breadcrumb-item active">Login or sign up</li>
            </ol>
        </nav>
        <div class="h-100 container" id="sessions-new">
            <div class="row">
                <div class="col-sm-6 mt-4" id="login_panel">
                    <h4 class="mb-3">Login</h4>
                    {renderLoginForm loginUser}
                </div>
                <div class="col-sm-6 mt-4" id="signup_panel">
                    <h4 class="mb-3">Or create a new account</h4>
                    {renderSignupForm signupUser}
                </div>
            </div>
        </div>
    |]

renderLoginForm :: User -> Html
renderLoginForm user =
    [hsx|
    <form id="login_form" method="POST" action={CreateSessionAction}>
        <div class="form-group">
            <label for="email">Email</label>
            <input name="email" value={get #email user} type="email" class="form-control" placeholder="Email" required="required" autofocus="autofocus" />
        </div>
        <div class="form-group">
            <label for="password">Password</label>
            <input name="password" type="password" class="form-control" placeholder="Password"/>
        </div>
        <button type="submit" class="btn btn-primary btn-block mt-4">Login</button>
    </form>
|]

renderSignupForm :: User -> Html
renderSignupForm user =
    formFor
        user
        [hsx|
    <form id="signup_form" method="POST" action={CreateUserAction}>
        {(emailField #email) {
            fieldLabel = "Email (will be used for pulling the userpic from Gravatar)"
        }}
        {textField #handle}
        {(textField #displayName) {
            fieldLabel = "Display name"
        }}
        {(passwordField #passwordHash){
            fieldLabel = "Password"
        }}
        <button class="btn btn-primary btn-block mt-4" type="submit">Create a new account</button>
    </form>
|]
