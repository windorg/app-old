module Web.View.CardUpdate.New where
import Web.View.Prelude

data NewView = NewView { cardUpdate :: CardUpdate }

instance View NewView where
    html NewView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={CardUpdatesAction}>CardUpdates</a></li>
                <li class="breadcrumb-item active">New CardUpdate</li>
            </ol>
        </nav>
        <h1>New CardUpdate</h1>
        {renderForm cardUpdate}
    |]

renderForm :: CardUpdate -> Html
renderForm cardUpdate = formFor cardUpdate [hsx|
    {(textField #content)}
    {(textField #cardId)}
    {submitButton}
|]
