module Web.View.Card.New where
import Web.View.Prelude

data NewView = NewView { card :: Card }

instance View NewView where
    html NewView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={CardsAction}>Cards</a></li>
                <li class="breadcrumb-item active">New Card</li>
            </ol>
        </nav>
        <h1>New Card</h1>
        {renderForm card}
    |]

renderForm :: Card -> Html
renderForm card = formFor card [hsx|
    {(textField #title)}
    {(textField #boardId)}
    {submitButton}
|]
