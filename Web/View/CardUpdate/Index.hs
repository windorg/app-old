module Web.View.CardUpdate.Index where
import Web.View.Prelude

data IndexView = IndexView { cardUpdate :: [CardUpdate] }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item active"><a href={CardUpdatesAction}>CardUpdates</a></li>
            </ol>
        </nav>
        <h1>Index <a href={pathTo NewCardUpdateAction} class="btn btn-primary ml-4">+ New</a></h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>CardUpdate</th>
                        <th></th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach cardUpdate renderCardUpdate}</tbody>
            </table>
        </div>
    |]


renderCardUpdate :: CardUpdate -> Html
renderCardUpdate cardUpdate = [hsx|
    <tr>
        <td>{cardUpdate}</td>
        <td><a href={ShowCardUpdateAction (get #id cardUpdate)}>Show</a></td>
        <td><a href={EditCardUpdateAction (get #id cardUpdate)} class="text-muted">Edit</a></td>
        <td><a href={DeleteCardUpdateAction (get #id cardUpdate)} class="js-delete text-muted">Delete</a></td>
    </tr>
|]
