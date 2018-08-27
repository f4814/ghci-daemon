{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.GhciDaemon.Server
    ( Daemon(..)
    , Settings(..)
    , mkDaemon
    , runDaemon
    , killDaemon
    , ghciDaemon
    ) where

import           Control.Concurrent (forkIO)
import           Control.Exception (bracket)
import           Control.Monad (forever)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import           Data.Semigroup ((<>))
import           Language.Haskell.Ghcid (Ghci, startGhci, interrupt)
import qualified Language.Haskell.Ghcid as Ghcid
import           Network.Socket hiding (send, sendTo, recv, recvFrom)
import           Network.Socket.ByteString (send, recv)
import           Options.Applicative
import           System.Posix.Daemon (Redirection(DevNull), runDetached)

-- |Options passed to the Server (e.g as cli arguments)
data Settings = Settings
    { port     :: Int
    , cmd      :: String
    , nofork   :: Bool
    }

-- |All information needed to run the server
data Daemon = Daemon
    { dSettings :: Settings
    , dSocket   :: Socket
    , dGhci     :: Ghci
    }

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
        <> value "stack ghci")
    <*> switch
         ( long "no-fork"
        <> short 'n'
        <> help "Do not run in background")

ghciDaemon :: IO ()
ghciDaemon = do
    d <- mkDaemon
    let act = bracket (return d) killDaemon runDaemon
    if nofork . dSettings $ d
        then act
        else runDetached Nothing DevNull act

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
    _ <- forkIO $ thread conn
    return ()
    where
        thread c = do
            msg <- recv c 4096
            res <- exec ghci msg
            _ <- send c res
            close c

-- |Execute command the apropriate way (take action on deamon, or send to ghci
-- |process
exec :: Ghci -> BS.ByteString -> IO BS.ByteString
exec ghci msg
    | msg == "GHCI-DAEMON: restart" = undefined
    | msg == "GHCI-DAEMON: interrupt" = interrupt ghci >> return "Interrupted"
    | otherwise = (BC.pack . unlines) <$> Ghcid.exec ghci (BC.unpack msg)

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
    listen sock 2
    return sock
        where
            hints = defaultHints {
                 addrFlags = [AI_PASSIVE]
               , addrSocketType = Stream
               }
