{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.GhciDaemon.Server
    ( Settings(..)
    , mkDaemon
    , runDaemon
    , killDaemon
    ) where

import           Control.Monad (forever)
import qualified Data.ByteString.Char8 as BC
import           Language.Haskell.Ghcid (Ghci, startGhci, exec, interrupt)
import           Network.Socket hiding (send, sendTo, recv, recvFrom)
import           Network.Socket.ByteString (send, recv)

-- |Options passed to the Server (e.g as cli arguments)
data Settings = Settings
    { port     :: Int
    , cmd      :: String
    }

-- |All information needed to run the server
data Daemon = Daemon Settings Socket Ghci

-- |Create a daemon
mkDaemon :: Settings -> IO Daemon
mkDaemon s = do
    sock <- openSocket s
    (ghci, msg) <- startGhci (cmd s) Nothing (\_ -> putStrLn)
    print msg
    return (Daemon s sock ghci)

-- |Run a daemon
runDaemon :: Daemon -> IO ()
runDaemon (Daemon _ sock ghci) = forever $ do
    (conn, _) <- accept sock
    msg <- BC.unpack <$> recv conn 4096
    res <- exec ghci msg
    _ <- send conn (BC.pack . unlines $ res)
    close conn

-- |Try to stop gracefully. Kill ghci if this does not succeed.
killDaemon :: Daemon -> IO ()
killDaemon (Daemon _ sock ghci) = do
    close sock
    interrupt ghci

-- |Open the network socket
openSocket :: Settings -> IO Socket
openSocket s = do
    addr:_ <- getAddrInfo (Just hints) Nothing (Just . show $ port s)
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    setSocketOption sock ReuseAddr 1
    bind sock (addrAddress addr)
    listen sock 1
    return sock
        where
            hints = defaultHints {
                 addrFlags = [AI_PASSIVE]
               , addrSocketType = Stream
               }
