module Web.View.User.New where
import Web.View.Prelude

data NewView = NewView { user :: User }

instance View NewView where
    html NewView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item active">New User</li>
            </ol>
        </nav>
        <h1>New User</h1>
        {renderForm user}
    |]

renderForm :: User -> Html
renderForm user = formFor user [hsx|
    {(textField #email)}
    {(textField #handle)}
    {(textField #displayName)}
    {(passwordField #passwordHash) {fieldLabel = "Password"}}
    {submitButton}
|]
