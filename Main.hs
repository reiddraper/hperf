module Main where

import Prelude hiding (null)

import Data.ByteString (null, ByteString)

import System.Environment (getArgs)
import System.Entropy (getEntropy)
import qualified Network.Socket as S
import Network.Socket.ByteString (sendAll, recv)

usage :: IO ()
usage = putStrLn "Usage"

message :: IO ByteString
message = getEntropy 1500

clientLoop :: S.Socket -> ByteString -> IO ()
clientLoop sock msg = do
    () <- sendAll sock msg
    clientLoop sock msg

client :: IO ()
client = do
    msg <- message
    sock <- S.socket S.AF_INET S.Stream S.defaultProtocol
    host <- S.inet_addr "127.0.0.1"
    S.connect sock $ S.SockAddrInet 5001 host
    clientLoop sock msg

serverLoop :: S.Socket -> IO ()
serverLoop sock = do
    bytes <- recv sock 4096
    if null bytes
       then error "Timeout"
       else serverLoop sock

server :: IO ()
server = do
    sock <- S.socket S.AF_INET S.Stream S.defaultProtocol
    S.setSocketOption sock S.ReuseAddr 1
    host <- S.inet_addr "127.0.0.1"
    S.bind sock $ S.SockAddrInet 5001 host
    S.listen sock 10
    (clientSock, _clientAddr) <- S.accept sock
    serverLoop clientSock

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-c"]  -> client
        ["-s"]  -> server
        _       -> usage
