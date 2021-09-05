module Web.View.Reply.New where
import Web.View.Prelude
import Web.Helper.ReplySource

data NewView = NewView { replySource :: ReplySource, reply :: Reply }

instance View NewView where
    html NewView { .. } = renderModal Modal
        { modalTitle = "New reply"
        , modalCloseUrl = pathToReplySource replySource
        , modalFooter = Nothing
        , modalContent = [hsx|
            {renderForm reply replySource}
        |]
        }

renderForm :: Reply -> ReplySource -> Html
renderForm reply replySource = formFor' reply (pathTo action) [hsx|
    {(textField #content)}
    {submitButton}
  |]
  where
    action = CreateReplyAction (get #cardUpdateId reply) (show replySource)
