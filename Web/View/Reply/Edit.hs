module Web.View.Reply.Edit where
import Web.View.Prelude
import Web.Helper.ReplySource

data EditView = EditView { replySource :: ReplySource, reply :: Reply }

instance View EditView where
    html EditView { .. } = renderModal Modal
      { modalTitle = "Edit reply"
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
    action = UpdateReplyAction (get #id reply) (show replySource)
