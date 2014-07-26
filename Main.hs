module Main where

import Prelude hiding (null)

import Data.ByteString (null, ByteString)

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically)
import qualified Control.Concurrent.STM.TBQueue as TBQueue
import Control.Monad (unless, forever, void)
import GHC.IO.Exception (IOErrorType(ResourceVanished))
import System.Entropy (getEntropy)
import System.Environment (getArgs)
import System.IO.Error (catchIOError, ioeGetErrorType)

import Network.Socket.ByteString (sendAll, recv)
import qualified Network.Socket as S

data Message = NewClient | ClientDisconnected deriving (Show)

usage :: IO ()
usage = putStrLn "Usage"

message :: IO ByteString
message = getEntropy 1500

clientLoop :: S.Socket -> ByteString -> IO ()
clientLoop sock msg = do
    () <- sendAll sock msg
    clientLoop sock msg

handleIOError :: IOError -> IO ()
handleIOError e = unless (ioeGetErrorType e == ResourceVanished) $ ioError e

client :: IO ()
client = do
    msg <- message
    sock <- S.socket S.AF_INET S.Stream S.defaultProtocol
    host <- S.inet_addr "127.0.0.1"
    S.connect sock $ S.SockAddrInet 5001 host
    catchIOError (clientLoop sock msg) handleIOError

serverThreadLoop :: TBQueue.TBQueue Message -> S.Socket -> IO ()
serverThreadLoop queue sock = do
    bytes <- recv sock 4096
    if null bytes
       then atomically $ TBQueue.writeTBQueue queue ClientDisconnected
       else serverThreadLoop queue sock

serverLoop :: TBQueue.TBQueue Message -> S.Socket -> IO ()
serverLoop queue sock = do
    (clientSock, _clientAddr) <- S.accept sock
    _threadID <- forkIO $ serverThreadLoop queue clientSock
    () <- atomically $ TBQueue.writeTBQueue queue NewClient
    serverLoop queue sock

server :: IO ()
server = do
    sock <- S.socket S.AF_INET S.Stream S.defaultProtocol
    S.setSocketOption sock S.ReuseAddr 1
    host <- S.inet_addr "127.0.0.1"
    S.bind sock $ S.SockAddrInet 5001 host
    S.listen sock 16
    queue <- atomically $ TBQueue.newTBQueue 16
    _threadID <- logLoop queue
    serverLoop queue sock

logLoop :: TBQueue.TBQueue Message -> IO ()
logLoop queue = void $ forkIO $ forever $ do
    msg <- atomically $ TBQueue.readTBQueue queue
    putStrLn $ "LOG: " ++ show msg

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-c"]  -> client
        ["-s"]  -> server
        _       -> usage
