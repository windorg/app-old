module Application.Orphans where

import Control.Monad (replicateM)
import Data.String.Conversions (cs)
import IHP.ModelSupport
import Text.ParserCombinators.ReadP (skipSpaces)
import Text.Read
import Prelude

instance ParsePrimaryKey (PrimaryKey model) => Read (Id' model) where
    readPrec = do
        lift skipSpaces
        xs <- replicateM 36 get
        case parsePrimaryKey (cs xs) of
            Just x -> pure (Id x)
            Nothing -> pfail
