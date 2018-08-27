module Language.Haskell.GhciDaemon.Client
    ( Settings(..)
    , Client(..)
    , mkClient
    , runClient
    , closeClient
    , exec
    , ghciClient
    ) where

import           Control.Monad (when)
import           Control.Monad.Loops (untilM_)
import           Control.Exception (bracket)
import qualified Data.ByteString.Char8 as BC
import           Data.Semigroup ((<>))
import           Options.Applicative
import           Network.Socket hiding (send, sendTo, recv, recvFrom)
import           Network.Socket.ByteString (sendAll, recv)
import           System.IO (BufferMode(..), stdin, hIsEOF, hSetBuffering)

-- |CLI Options
data Settings = Settings
    { port :: Int
    , repl :: Bool
    , free :: Bool
    , cmd  :: String
    , host :: String
    }

-- |Everything needed to send a request to the server
data Client = Client
    { cSettings :: Settings
    , cSocket   :: Socket
    }

settings :: Parser Settings
settings = Settings
    <$> option auto
         ( long "port"
        <> short 'p'
        <> help "The port to connect to"
        <> metavar "PORT"
        <> value 3497)
    <*> switch
         ( long "repl"
        <> short 'r'
        <> help "Start a REPL")
    <*> switch
         ( long "free"
        <> short 'f'
        <> help "Free the ghci process")
    <*> strOption
         ( long "cmd"
        <> short 'c'
        <> help "Execute command"
        <> metavar "CMD"
        <> value "")
    <*> strOption
         ( long "host"
        <> short 'h'
        <> help "Host to connect to"
        <> metavar "HOSTNAME"
        <> value "127.0.0.1")

-- |Open the socket
openSocket :: Settings -> IO Socket
openSocket s = do
    addr:_ <- getAddrInfo (Just hints) (Just . host $ s) (Just . show . port $ s)
    sock   <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    connect sock $ addrAddress addr
    return sock
        where
            hints = defaultHints { addrSocketType = Stream }

-- |Parse all cli args and open socket
mkClient :: IO Client
mkClient = do
    s <- execParser opts
    sock <- openSocket s
    return $ Client s sock
        where
            opts = info (settings <**> helper)
                 ( fullDesc
                <> progDesc "Connect to a ghci daemon"
                <> header "ghci-client") 

-- |Execute a Client
runClient :: Client -> IO ()
runClient c = do
    when ((free . cSettings $ c)) $
        exec (cSocket c) "GHCI-DAEMON: interrupt"
    when ((cmd . cSettings $ c) /= "") $
        exec (cSocket c) (cmd . cSettings $ c)
    when (repl . cSettings $ c) $ do
        hSetBuffering stdin NoBuffering
        putStrLn "Starting remote repl"
        runRepl (cSocket c)

-- |Send a command to server and print result to stdout
exec :: Socket -> String -> IO ()
exec s c = do
    sendAll s (BC.pack c)
    recv s 4096 >>= BC.putStrLn

-- |Run a basic repl
runRepl :: Socket -> IO ()
runRepl s = prompt `untilM_` (hIsEOF stdin)
    where
        prompt = do
            c <- getLine
            exec s c

-- |Parse and execute all cli arguments
ghciClient :: IO ()
ghciClient = do
    bracket mkClient closeClient runClient

-- |Close connection to server
closeClient :: Client -> IO ()
closeClient Client {cSocket = sock, cSettings = _} = do
    close sock
