module Web.View.Card.Index where
import Web.View.Prelude

data IndexView = IndexView { card :: [Card] }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item active"><a href={CardsAction}>Cards</a></li>
            </ol>
        </nav>
        <h1>Index <a href={pathTo NewCardAction} class="btn btn-primary ml-4">+ New</a></h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>Card</th>
                        <th></th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach card renderCard}</tbody>
            </table>
        </div>
    |]


renderCard :: Card -> Html
renderCard card = [hsx|
    <tr>
        <td>{card}</td>
        <td><a href={ShowCardAction (get #id card)}>Show</a></td>
        <td><a href={EditCardAction (get #id card)} class="text-muted">Edit</a></td>
        <td><a href={DeleteCardAction (get #id card)} class="js-delete text-muted">Delete</a></td>
    </tr>
|]
