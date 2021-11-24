module Config where

import IHP.Environment
import IHP.FrameworkConfig
import IHP.Prelude
import IHP.Sentry

config :: ConfigBuilder
config = do
    option Development
    option (AppHostname "localhost")

-- initSentry "https://595bd2e4e0b246ddaabbd651750f3512@o1035259.ingest.sentry.io/6002058"
