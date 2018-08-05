{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.GhciDaemon.Server
    ( Daemon(..)
    , startServer
    , killServer
    , server
    ) where

import Options.Applicative (Parser(..), strOption, long, short, metavar, help)
import Data.Semigroup ((<>))
import System.Posix.Daemon (Redirection(..), runDetached)
import System.Process (CreateProcess(..), StdStream(..), ProcessHandle(..), createProcess, shell, withCreateProcess)
import System.IO (Handle(), BufferMode(..), hIsEOF, hGetLine, hPutStr, hPutStrLn, hSetBuffering, hFlush)
import Control.Monad (unless)
import Control.Concurrent (threadDelay)
import Pipes
import Pipes.Core
import Pipes.ByteString (toHandle, fromHandle)
import qualified Pipes.ByteString as P
import qualified Pipes.Prelude as P
import Pipes.Network.TCP (fromSocket, toSocket)
import Network.Socket --hiding (send, sendTo, recv, recvFrom)
-- import Network.Socket.ByteString
import Control.Monad (forever)
import Control.Concurrent (forkIO)
import qualified Data.ByteString as BS
import Language.Haskell.Ghcid
import qualified Data.ByteString.Char8 as BC

-- |Options passed to the Server
data Daemon = Daemon
    { port     :: Int
    , ghciFile :: Maybe FilePath
    , pidFile  :: FilePath
    , cmd      :: String
    }

-- daemon :: Parser Daemon
-- daemon = Daemon
--     <$> strOption
--          ( long "port"
--         <> short 'p'
--         <> metavar "PORT"
--         <> help "port to bind to")
--     <*> strOption
--          ( long "ghciFile"
--         <> short 'f'
--         <> metavar "FILE"
--         <> help "ghci file to load at startup")
--     <*> strOption
--          ( long "pidFile"
--         <> metavar "FILE"
--         <> help "pidfile to use")

-- |Start the server process
startServer :: Daemon -> IO ()
-- startServer d = runDetached (Just $ pidFile d) (ToFile "test") (server d)
startServer d = do
    sock <- openSocket d
    (ghci, msg) <- startGhci (cmd d) Nothing (\_ -> putStrLn)
    print msg
    server sock d ghci

-- |Server process
server :: Socket -> Daemon -> Ghci -> IO ()
server s d g = forever $ do
    (conn, _) <- accept s
    cmd <- recv conn 4096
    res <- exec g cmd
    send conn (unlines res)
    close conn

-- |Open the network socket
-- TODO reade Daemon
openSocket :: Daemon -> IO Socket
openSocket d = do
    addr:_ <- getAddrInfo (Just hints) Nothing (Just "5000")
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

-- |Main loop
mainLoop :: Socket -> Handle -> Handle -> Handle -> IO ()
mainLoop sock inp out err = forever $ do
    putStrLn "Loop"
    (conn, peer) <- accept sock
    runEffect $ fromSocket conn 4096 >-> ghciConsumer inp
    runEffect $ ghciProducer out err >-> toSocket conn
    close conn

ghciProducer :: Handle -> Handle -> Producer BS.ByteString IO ()
ghciProducer h e = do
    str <- lift $ BS.hGetLine h
    err <- lift $ BS.hGetLine e
    lift $ BS.putStrLn str
    lift . BS.putStrLn $ "ERR: " <> err
    yield str
    yield "\n"
    if str `contains` "~#GHCI-DAEMON#~"
        then lift $ return ()
        else do
            ghciProducer h e

ghciConsumer :: Handle -> Consumer BS.ByteString IO ()
ghciConsumer h = do
    str <- await
    lift . BS.hPutStrLn h $ str

contains :: BS.ByteString -> BS.ByteString -> Bool
contains m s = (snd $ BS.breakSubstring s m) /= ""

killServer :: IO ()
killServer = undefined
