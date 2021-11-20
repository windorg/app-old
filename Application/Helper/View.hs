module Application.Helper.View where

import IHP.ViewPrelude
import qualified CMark -- using instead of commonmark because the latter does not support 'safe'
import Fmt

-- Here you can add functions which are available in all your views

renderMarkdown :: Text -> Html
renderMarkdown text = preEscapedToHtml (CMark.commonmarkToHtml [CMark.optSafe] text)

renderTimestamp :: UTCTime -> Text
renderTimestamp time =
    -- February 14th, 2021, 18:20 UTC
    format "{} {}, {}, {} UTC"
      (timeF "%B" time)
      (dayOfMonthOrdF time)
      (yearF time)
      (timeF "%R" time)