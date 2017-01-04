{-# LANGUAGE OverloadedStrings,BangPatterns, MultiWayIf #-}
module Main where

import Control.Monad
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Exception
import Data.Monoid
import System.Random
import System.Environment


import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy  as L
import qualified Data.Vector           as V
import qualified Data.Text             as T
import qualified Data.Text.IO          as T.IO
import qualified Data.Text.Encoding    as T.E
import qualified Network.Simple.TCP    as Net
import qualified Network.HTTP          as HTTP
import qualified Network.Stream        as NS
import qualified Data.Aeson            as JSON

--base_url = "https://qwebirc.swiftirc.net/"

s2b :: String -> C8.ByteString
s2b = T.E.encodeUtf8 . T.pack

b2s :: C8.ByteString -> String
b2s = T.unpack . T.E.decodeUtf8

data Client = MkClient {
                base_url :: !C8.ByteString,
                tim      :: !(MVar Integer),
                session  :: !T.Text
}


data RequestError = ParsingError !String !String
                  | ConnectError !NS.ConnError
    deriving(Show)

request :: JSON.FromJSON ret => Client -> Char -> [(String, String)] -> IO (Either RequestError ret)
request (MkClient base_url tim _) char params = do
        !n <- takeMVar tim
        putMVar tim (n+1)
        let getParams = "?" ++ HTTP.urlEncodeVars [("r", "1337"), ("t", show n)]
                                {-^ awfulhack? 'causeIdon'tknowhow2http-}
        let url = b2s base_url ++ "e/" ++ [char] ++ getParams
        ret <- HTTP.simpleHTTP (HTTP.postRequestWithBody url "application/x-www-form-urlencoded" (HTTP.urlEncodeVars params))
        case ret of
             Left e -> return (Left (ConnectError e))
             Right (HTTP.Response {HTTP.rspBody=ret}) -> do
                let v = JSON.decode . L.fromStrict . s2b $ ret
                case v of Nothing -> return (Left (ParsingError url ret))
                          Just x  -> return (Right x)


connect :: C8.ByteString -> String -> IO (Either RequestError Client)
connect base_url nick = do
    tim <- newMVar 0
    let tmpClient = MkClient base_url tim "<ignored>"
    session <- request tmpClient 'n' [("nick", nick)]
    return $ do (_:JSON.String sess_id:_) <- session
                return (MkClient base_url tim sess_id)

send :: Client -> T.Text -> IO (Either RequestError ())
send client payload = do
    request client 'p' [("s", T.unpack $ session client), ("c", T.unpack payload)]


data Message = Message    !T.Text
             | Disconnect !T.Text
             | Connect
             | Err        ![JSON.Value] !String
    deriving (Show)
recv :: Client -> IO (Either RequestError [Message])
recv client = do
    ret <- request client 's' [("s", T.unpack $ session client)]
    return $ fmap recv_magic ret

recv_magic :: [[JSON.Value]] -> [Message]
recv_magic xs = map f xs where
    f x@[JSON.String "c",JSON.String command,JSON.String prefix,JSON.Array args] =
        go x ((if T.null prefix then "" else ":" <> prefix <> " ") <>
               command) (V.toList args)
    f [JSON.String "disconnect" , JSON.String message] =
        Disconnect message
    f [JSON.String "connect"] =
        Connect
    f x = Err x "Unknown message type"
            {- should have a custom datatype for parsing this stuff -}
    go !x !acc []                       = Message acc
    go !x !acc [JSON.String txt] | T.count " " txt > 0
                                        = go x (acc <> " :" <> txt) []
    go !x !acc (JSON.String txt : rest) = go x (acc <> " "  <> txt) rest
    go !x !acc _ = Err x "Invalid c structure"

main = do args <- getArgs
          case args of
               [host, port, prefix, "quiet" ] -> loop Errors  host port prefix
               [host, port, prefix, "simple"] -> loop Simple  host port prefix
               [host, port, prefix, "all"   ] -> loop Verbose host port prefix
               [host, port, prefix] -> loop Simple host port prefix
               _ -> mapM_ putStrLn ["Example usage:"
                                   ,"\t./relay http://webchat.network.net/ 1337 nickprefix_"
                                   ,"you can optionally add a third parameter that sets the level of info you wanna get in stdout"
                                   ,"\tall    - all messages"
                                   ,"\tsimple - connection/disconnection and errors"
                                   ,"\tquiet  - only errors"
                                   ,"Invalid parameters will send you here!"]

data DLevel = Errors
            | Simple
            | Verbose
    deriving (Show,Ord,Eq)

loop :: DLevel -> String -> String -> String -> IO ()
loop curr host port prefix =
    Net.withSocketsDo $ Net.serve (Net.Host "0.0.0.0") port $ \(sock, remAddr) -> do
            nick <- (\n -> prefix ++ show n) <$> randomRIO (1337,9001 :: Int)
            let logN l s = when (curr >= l) $
                               putStrLn $ "[" ++ show remAddr ++ "/"++ nick ++"] "++ s

            logN Simple "connecting..."
            cl <- connect (s2b host) nick
            case cl of
                 Left e -> do logN Errors $ "error: " ++ show e
                 Right cl -> do
                        thId <- forkIO $ reader logN cl sock
                        writer logN cl sock `finally` (do
                                    killThread thId
                                    send cl "QUIT :killin' the mood"
                                    threadDelay 25000
                                    Net.closeSock sock
                                    logN Simple "disconnected")
    where
        writer logN client sock = do
            x <- recv client
            case x of
                 Left e -> logN Errors $ "error: " ++ show e
                 Right xp -> do
                    l <- fmap (all id) $ forM xp $ \x' -> do
                        case x' of
                             Connect -> do
                                 logN Simple $ "IRC server confirmed connection"
                                 return True
                             Err e msg -> do
                                 logN Errors $ "error: " ++ show msg ++ show " on " ++ show e
                                 return True
                             Disconnect m -> do
                                    logN Simple $
                                        "disconnect msg from IRC server [" ++ show m ++ "]"
                                    return False
                             Message msg -> do
                                    logN Verbose $ "Read from IRC: " ++ T.unpack msg
                                    Net.send sock (T.E.encodeUtf8 msg <> "\n")
                                    return True
                    when l (writer logN client sock)
            
        reader logN client sock = do
            x <- readLineFrom sock
            case x of
                 Nothing -> () <$ send client "QUIT :killin' the mod, twice."
                 Just x' -> do logN Verbose ("Read from socket: " ++ show x')
                               send client (T.E.decodeUtf8 x')
                               reader logN client sock

readLineFrom :: Net.Socket -> IO (Maybe C8.ByteString)
readLineFrom sock = do
    c <- Net.recv sock 1
    case c of Nothing -> return Nothing
              Just c' -> Just <$> go' c'
    where go' :: C8.ByteString -> IO C8.ByteString
          go' c | c == "\n" || c == "" = return ""
                | c == "\r" = do
                        Net.recv sock 1 {- we hope there's always a \n after a \r -}
                        return ""
                | otherwise = (\y -> c <> maybe "" id y) <$> readLineFrom sock
