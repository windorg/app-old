{-# LANGUAGE ViewPatterns #-}

module Web.View.Prelude (
    module IHP.ViewPrelude,
    module Web.View.Layout,
    module Generated.Types,
    module Web.Types,
    module Application.Helper.View,
    module Web.Helper.Common,
    module Web.View.Prelude,
    module Web.View.Modal,
    module Optics,
    module Fmt,
) where

import Application.Helper.View
import Generated.Types
import IHP.ViewPrelude hiding (renderModal, renderModal')
import Web.Helper.Common
import Web.Routes ()
import Web.Types
import Web.View.Layout
import Web.View.Modal (renderModal, renderModal')

import Fmt (format)
import Optics ((%), (^.))

import qualified Text.Blaze.Html5.Attributes as H
import Text.Blaze.Internal (customAttribute)

cmdEnterSubmit :: FormField -> FormField
cmdEnterSubmit f =
    f
        { fieldInput = \formField ->
            fieldInput f f
                ! H.onkeydown
                    "if (event.keyCode == 13 && (event.metaKey || event.ctrlKey)) {\
                    \$(this).closest('form').submit()} "
        }
