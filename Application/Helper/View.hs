module Application.Helper.View where

import IHP.ViewPrelude
import qualified CMark -- using instead of commonmark because the latter does not support 'safe'
import Fmt

-- Here you can add functions which are available in all your views

renderMarkdown :: Text -> Html
renderMarkdown text = preEscapedToHtml (CMark.commonmarkToHtml [CMark.optSafe] text)

-- See the sad story in https://windofchange.me/ShowCard?cardId=5bde1f3a-2b30-4085-bc04-b421eb3051ce#comment-1f6e131e-5ea5-4c19-8330-8a6783e542bd
renderTimestamp :: UTCTime -> Text
renderTimestamp time =
    -- February 14th, 2021, 18:20 UTC
    format "{} {}, {}, {} UTC"
      (timeF "%B" time)
      (dayOfMonthOrdF time)
      (yearF time)
      (timeF "%R" time)