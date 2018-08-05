module Main where

import           Language.Haskell.GhciDaemon.Server
import           Control.Exception (bracket)

main :: IO ()
main = bracket (mkDaemon d) killDaemon runDaemon
    where d = Settings 4444 "stack ghci ghci-daemon:exe:ghci-daemon"
