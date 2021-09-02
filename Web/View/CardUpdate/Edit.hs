module Web.View.CardUpdate.Edit where
import Web.View.Prelude

data EditView = EditView { card :: Card, cardUpdate :: CardUpdate }

instance View EditView where
    html EditView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={BoardsAction}>Boards</a></li>
                <li class="breadcrumb-item">
                  <a href={ShowCardAction (get #cardId cardUpdate) }>
                    {get #title card}
                  </a>
                </li>
                <li class="breadcrumb-item active">Edit update</li>
            </ol>
        </nav>
        <h1>Edit update</h1>
        {renderForm cardUpdate}
    |]

renderForm :: CardUpdate -> Html
renderForm cardUpdate = formFor cardUpdate [hsx|
    {(textareaField #content) {
        disableLabel = True
      } |> autosize 5
    }
    {submitButton {label = "Save"}}
|]
