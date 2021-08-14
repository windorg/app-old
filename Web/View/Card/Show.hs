module Web.View.Card.Show where
import Web.View.Prelude

data ShowView = ShowView { card :: Card }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={CardsAction}>Cards</a></li>
                <li class="breadcrumb-item active">Show Card</li>
            </ol>
        </nav>
        <h1>Show Card</h1>
        <p>{card}</p>
    |]
