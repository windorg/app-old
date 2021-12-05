module Web.View.Modal where

import IHP.ViewPrelude hiding (renderModal)
import Text.Blaze.Html5 (preEscapedText)

-- Like the official modal, but doesn't close on backdrop clicks

renderModal modal = renderModal' modal True
renderModal' Modal{..} show =
    [hsx|
        <div class={modalClassName} id="modal" tabindex="-1" role="dialog" aria-labelledby="modal-title" aria-hidden="true" style={displayStyle}>
            {modalInner}
        </div>
        <a id="modal-backdrop" class={backdropClassName} style={displayStyle}/>
    |]
  where
    modalClassName :: Text
    modalClassName = "modal fade overflow-auto " <> if show then "show" else ""
    backdropClassName :: Text
    backdropClassName = "modal-backdrop fade " <> if show then "show" else ""
    displayStyle :: Text
    displayStyle = if show then "display: block" else "display: none"

    modalInner =
        [hsx|
            <div class="modal-dialog" role="document" id="modal-inner">
                <div class="modal-content">
                    {renderModalHeader modalTitle modalCloseUrl}
                    <div class="modal-body">{modalContent}</div>
                    {renderModalFooter}
                </div>
            </div>
            |]

    renderModalFooter =
        case modalFooter of
            Just modalFooter -> [hsx|<div class="modal-footer">{modalFooter}</div>|]
            Nothing -> mempty

renderModalHeader :: Text -> Text -> Html
renderModalHeader title closeUrl =
    [hsx|
    <div class="modal-header">
      <h5 class="modal-title" id="modal-title">{title}</h5>
      <a href={closeUrl} class="btn-link close" data-dismiss="modal" aria-label="Close">
        <span aria-hidden="true">{preEscapedText "&times;"}</span>
      </a>
    </div>
|]