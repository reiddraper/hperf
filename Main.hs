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

type MessageQueue = TBQueue.TBQueue Message

usage :: IO ()
usage = putStrLn "Usage"

message :: IO ByteString
message = getEntropy 1500

tcpSocket :: IO S.Socket
tcpSocket = S.socket S.AF_INET S.Stream S.defaultProtocol

clientLoop :: S.Socket -> ByteString -> IO ()
clientLoop sock msg = do
    () <- sendAll sock msg
    clientLoop sock msg

handleIOError :: IOError -> IO ()
handleIOError e = unless (ioeGetErrorType e == ResourceVanished) $ ioError e

connectSocket :: String -> IO S.Socket
connectSocket ip4StringAddr = do
    sock <- tcpSocket
    host <- S.inet_addr ip4StringAddr
    S.connect sock $ S.SockAddrInet 5001 host
    return sock

client :: IO ()
client = do
    msg <- message
    sock <- connectSocket "127.0.0.1"
    catchIOError (clientLoop sock msg) handleIOError

serverThreadLoop :: MessageQueue -> S.Socket -> IO ()
serverThreadLoop queue sock = do
    bytes <- recv sock 4096
    if null bytes
       then atomically $ TBQueue.writeTBQueue queue ClientDisconnected
       else serverThreadLoop queue sock

serverLoop :: MessageQueue -> S.Socket -> IO ()
serverLoop queue sock = do
    (clientSock, _clientAddr) <- S.accept sock
    _threadID <- forkIO $ serverThreadLoop queue clientSock
    () <- atomically $ TBQueue.writeTBQueue queue NewClient
    serverLoop queue sock

createListenSocket :: String -> IO S.Socket
createListenSocket ip4StringAddr = do
    sock <- tcpSocket
    S.setSocketOption sock S.ReuseAddr 1
    host <- S.inet_addr ip4StringAddr
    S.bind sock $ S.SockAddrInet 5001 host
    S.listen sock 16
    return sock

server :: IO ()
server = do
    sock <- createListenSocket "127.0.0.1"
    queue <- atomically $ TBQueue.newTBQueue 16
    _threadID <- logLoop queue
    serverLoop queue sock

logLoop :: MessageQueue -> IO ()
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
