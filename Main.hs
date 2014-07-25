module Main where

import Prelude hiding (null)

import Data.ByteString (null, ByteString)

import Control.Monad (void, unless)
import GHC.IO.Exception (IOErrorType(ResourceVanished))
import System.Entropy (getEntropy)
import System.Environment (getArgs)
import System.IO.Error (catchIOError, ioeGetErrorType)

import Network.Socket.ByteString (sendAll, recv)
import qualified Network.Socket as S

usage :: IO ()
usage = putStrLn "Usage"

message :: IO ByteString
message = getEntropy 1500

clientLoop :: S.Socket -> ByteString -> IO Bool
clientLoop sock msg = doSend >>= \b -> if b then clientLoop sock msg else return True
    where doSend = catchIOError (sendAll sock msg >> return True) handleIOError
          handleIOError e = if ioeGetErrorType e == ResourceVanished
                               then return False
                               else ioError e

client :: IO ()
client = do
    msg <- message
    sock <- S.socket S.AF_INET S.Stream S.defaultProtocol
    host <- S.inet_addr "127.0.0.1"
    S.connect sock $ S.SockAddrInet 5001 host
    void $ clientLoop sock msg

serverLoop :: S.Socket -> IO ()
serverLoop sock = do
    bytes <- recv sock 4096
    unless (null bytes) $ serverLoop sock

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
