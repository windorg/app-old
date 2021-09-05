{-# LANGUAGE ViewPatterns #-}
module Web.View.Prelude
( module IHP.ViewPrelude
, module Web.View.Layout
, module Generated.Types
, module Web.Types
, module Application.Helper.View
, module Web.View.Prelude
) where

import IHP.ViewPrelude
import Web.View.Layout
import Generated.Types
import Web.Types
import Web.Routes ()
import Application.Helper.View

import Text.Blaze.Internal (customAttribute)
import qualified Text.Blaze.Html5.Attributes as H

autosize :: Int -> FormField -> FormField
autosize minRows f = f { 
    fieldInput = \formField -> 
        fieldInput f f 
            ! customAttribute "is" "auto-size" 
            ! H.rows (cs (show minRows))
    }

cmdEnterSubmit :: FormField -> FormField
cmdEnterSubmit f = f { 
    fieldInput = \formField -> 
        fieldInput f f 
            ! H.onkeydown 
                "if (event.keyCode == 13 && (event.metaKey || event.ctrlKey)) {\
                    \$(this).closest('form').submit()} "
    }
