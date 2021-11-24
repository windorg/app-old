module Web.View.User.Edit where

import Web.View.Prelude

data EditView = EditView {user :: User}

instance View EditView where
    html EditView{..} =
        [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item active">Edit User</li>
            </ol>
        </nav>
        <h1>Edit User</h1>
        {renderForm user}
    |]

renderForm :: User -> Html
renderForm user =
    formFor
        user
        [hsx|
    {(textField #email)}
    {(textField #handle)}
    {(textField #displayName)}
    {submitButton}
|]
