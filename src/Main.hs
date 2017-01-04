{-# LANGUAGE OverloadedStrings,BangPatterns #-}
module Main where

import Control.Monad
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Data.Monoid

import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy  as L
import qualified Data.Vector           as V
import qualified Data.Text             as T
import qualified Data.Text.IO          as T.IO
import qualified Data.Text.Encoding    as T.E
import qualified Network.HTTP as HTTP
import qualified Data.Aeson as JSON

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


request :: JSON.FromJSON ret => Client -> Char -> [(String, String)] -> IO (Maybe ret)
request (MkClient base_url tim _) char params = do
        !n <- takeMVar tim
        putMVar tim (n+1)
        let getParams = "?" ++ HTTP.urlEncodeVars [("r", "1337"), ("t", show n)]
                                {-^ awfulhack? 'causeIdon'tknowhow2http-}
        let url = b2s base_url ++ "e/" ++ [char] ++ getParams
        ret <- HTTP.simpleHTTP (HTTP.postRequestWithBody url "application/x-www-form-urlencoded" (HTTP.urlEncodeVars params))
        case ret of
             Left {} -> return Nothing
             Right (HTTP.Response {HTTP.rspBody=ret}) -> do
                putStrLn ("URL : " <> url)
                putStrLn ret
                return $ JSON.decode . L.fromStrict . s2b $ ret


connect :: C8.ByteString -> String -> IO Client
connect base_url nick = do
    tim <- newMVar 0
    let tmpClient = MkClient base_url tim "<ignored>"
    session <- request tmpClient 'n' [("nick", nick)]
    case session of
         Just (_:JSON.String sess_id:_) -> return $ MkClient base_url tim sess_id
         x -> error (show x)

send :: Client -> T.Text -> IO ()
send client payload = do
    request client 'p' [("s", T.unpack $ session client), ("c", T.unpack payload)] :: IO (Maybe ())
    return ()

recv :: Client -> IO [T.Text]
recv client = do
    ret <- request client 's' [("s", T.unpack $ session client)]
    case recv_magic =<< ret of
         Nothing -> return [] -- throwIO 
         Just r  -> return r

recv_magic :: [[JSON.Value]] -> Maybe [T.Text]
recv_magic xs = foldr (<>) mempty $ map (\x -> (:[]) <$> f x) xs where
    f [JSON.String "c",JSON.String command,JSON.String prefix,JSON.Array args] =
        go ((if T.null prefix then "" else ":" <> prefix <> " ") <>
               command) (V.toList args)
    f _ = Nothing
    go !acc []                       = Just acc
    go !acc [JSON.String txt]        = Just (acc <> " :" <> txt)
    go !acc (JSON.String txt : rest) = go (acc <> " " <> txt) rest
    

readLoop :: Client -> IO ()
readLoop cl = go where
    go = do x <- recv cl
            forM_ x $ \t -> do
                    T.IO.putStrLn (">>> " <> t)
            threadDelay (1000 * 50)
            go

writeLoop :: Client -> IO ()
writeLoop cl = go where
    go = do x <- T.IO.getLine
            send cl x
            go

main :: IO ()
main = do
    c <- connect "http://irc.w3.org/" "magicalclient2"
    forkIO (readLoop c)
    writeLoop c