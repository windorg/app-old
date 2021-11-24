module Main where

import IHP.Prelude

import Config
import IHP.FrameworkConfig
import IHP.Job.Types
import IHP.RouterSupport
import qualified IHP.Server
import Web.FrontController
import Web.Types

instance FrontController RootApplication where
    controllers =
        [ mountFrontController WebApplication
        ]

instance Worker RootApplication where
    workers _ = []

main :: IO ()
main = IHP.Server.run config
