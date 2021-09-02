module Web.View.Card.Edit where
import Web.View.Prelude

data EditView = EditView { card :: Card }

instance View EditView where
    html EditView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item active">Edit Card</li>
            </ol>
        </nav>
        <h1>Edit Card</h1>
        {renderForm card}
    |]

renderForm :: Card -> Html
renderForm card = formFor card [hsx|
    {(textField #title)}
    {submitButton}
|]
