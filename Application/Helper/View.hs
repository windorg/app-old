module Application.Helper.View where

import IHP.ViewPrelude
import qualified CMark -- using instead of commonmark because the latter does not support 'safe'
import Fmt

-- Here you can add functions which are available in all your views

renderMarkdown :: Text -> Html
renderMarkdown text = preEscapedToHtml (CMark.commonmarkToHtml [CMark.optSafe] text)

-- TODO render year as well
renderTimestamp :: UTCTime -> Text
renderTimestamp time =
    -- February 14th, 18:20
    format "{} {}, {}"
      (timeF "%B" time)
      (dayOfMonthOrdF time)
      (timeF "%R" time)