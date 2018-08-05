module Main where

import           Language.Haskell.GhciDaemon.Server
import           Control.Exception (bracket)

main :: IO ()
main = bracket mkDaemon killDaemon runDaemon
