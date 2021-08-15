module Web.View.Board.Show where
import Web.View.Prelude

data ShowView = ShowView { board :: Board, cards :: [Card] }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={BoardsAction}>Boards</a></li>
                <li class="breadcrumb-item active">{get #title board}</li>
            </ol>
        </nav>
        <h1>{get #title board}</h1>
        {forEach cards renderCard}
    |]
      where
        renderCard card = [hsx|
          <p>
            <a href={ShowCardAction (get #id card)}>{get #title card}</a>
          </p>
        |]
