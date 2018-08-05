{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.GhciDaemon.Server
    ( Settings(..)
    , mkDaemon
    , runDaemon
    , killDaemon
    ) where

import           Control.Monad (forever)
import qualified Data.ByteString.Char8 as BC
import           Data.Semigroup ((<>))
import           Language.Haskell.Ghcid (Ghci, startGhci, exec, interrupt)
import           Network.Socket hiding (send, sendTo, recv, recvFrom)
import           Network.Socket.ByteString (send, recv)
import           Options.Applicative

-- |Options passed to the Server (e.g as cli arguments)
data Settings = Settings
    { port     :: Int
    , cmd      :: String
    }

-- |All information needed to run the server
data Daemon = Daemon Settings Socket Ghci

settings :: Parser Settings
settings = Settings 
    <$> option auto
         ( long "port"
        <> short 'p'
        <> help "The port for the server to listen to"
        <> metavar "PORT"
        <> value 3497)
    <*> strOption
         ( long "cmd"
        <> short 'c'
        <> help "The command to run ghci"
        <> metavar "GHCICMD"
        <> value "stack ghci"
         )
-- |Create a daemon
mkDaemon :: IO Daemon
mkDaemon = do
    s <- execParser opts
    sock <- openSocket s
    (ghci, msg) <- startGhci (cmd s) Nothing (\_ -> putStrLn)
    print msg
    return (Daemon s sock ghci)
        where
            opts = info (settings <**> helper)
                 ( fullDesc
                <> progDesc "Run a ghci daemon"
                <> header "ghci-daemon")

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
