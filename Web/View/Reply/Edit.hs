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
          {renderForm reply}
        |]
      }

renderForm :: Reply -> Html
renderForm reply = formFor reply [hsx|
    {(textField #content)}
    {submitButton}
|]
