module Main where

import Language.Haskell.GhciDaemon.Server

main :: IO ()
main = startServer d
    where d = Daemon 4444 Nothing "pid" "stack ghci ghci-daemon:exe:ghci-daemon"
