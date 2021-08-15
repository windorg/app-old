module Web.View.CardUpdate.Edit where
import Web.View.Prelude

data EditView = EditView { cardUpdate :: CardUpdate }

instance View EditView where
    html EditView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={CardUpdatesAction}>CardUpdates</a></li>
                <li class="breadcrumb-item active">Edit CardUpdate</li>
            </ol>
        </nav>
        <h1>Edit CardUpdate</h1>
        {renderForm cardUpdate}
    |]

renderForm :: CardUpdate -> Html
renderForm cardUpdate = formFor cardUpdate [hsx|
    {(textField #content)}
    {(textField #cardId)}
    {submitButton}
|]
