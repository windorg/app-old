module Application.Helper.View where

import IHP.ViewPrelude
import qualified Commonmark
import Fmt

-- Here you can add functions which are available in all your views

renderMarkdown :: Text -> Html
renderMarkdown text =
    case Commonmark.commonmark "" text of
        Left err -> toHtml text
        Right (val :: Commonmark.Html ()) ->
            preEscapedToHtml (Commonmark.renderHtml val)

-- TODO render year as well
renderTimestamp :: UTCTime -> Text
renderTimestamp time =
    -- February 14th, 18:20
    format "{} {}, {}"
      (timeF "%B" time)
      (dayOfMonthOrdF time)
      (timeF "%R" time)