module Application.Orphans where

import IHP.ModelSupport
import Text.Read
import Prelude
import Data.String.Conversions (cs)
import Control.Monad (replicateM)
import Text.ParserCombinators.ReadP (skipSpaces)

instance ParsePrimaryKey (PrimaryKey model) => Read (Id' model) where
    readPrec = do
        lift skipSpaces
        xs <- replicateM 36 get
        case parsePrimaryKey (cs xs) of
          Just x -> pure (Id x)
          Nothing -> pfail