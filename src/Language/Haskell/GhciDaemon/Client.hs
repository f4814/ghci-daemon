module Language.Haskell.GhciDaemon.Client
    ( Client(..)
    ) where

-- |CLI Options
data Client = Client
    { port :: Int
    }
