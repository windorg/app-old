module Web.View.CardUpdate.Show where
import Web.View.Prelude

data ShowView = ShowView { cardUpdate :: CardUpdate }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={BoardsAction}>Boards</a></li>
                <li class="breadcrumb-item">CardUpdates</li>
                <li class="breadcrumb-item active">Show CardUpdate</li>
            </ol>
        </nav>
        <h1>Show CardUpdate</h1>
        <p>{cardUpdate}</p>
    |]
