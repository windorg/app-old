module Web.View.Card.Show where
import Web.View.Prelude

data ShowView = ShowView { card :: Card, cardUpdates :: [CardUpdate] }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={CardsAction}>Cards</a></li>
                <li class="breadcrumb-item active">Show Card</li>
            </ol>
        </nav>
        <h1>{get #title card}</h1>
        {forEach cardUpdates renderCardUpdate}
    |]
      where
        renderCardUpdate cardUpdate = [hsx|
          <p>
            <span class="text-muted small">{get #createdAt cardUpdate}</span><br>
            <a href={ShowCardUpdateAction (get #id cardUpdate)}>
              {get #content cardUpdate}
            </a>
          </p>
        |]
