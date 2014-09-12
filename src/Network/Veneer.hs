{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Network.Veneer(veneer) where


import qualified Blaze.ByteString.Builder as Builder
import           Control.Concurrent       (Chan, MVar, newChan, newEmptyMVar,
                                           putMVar, readChan, readMVar,
                                           writeChan)
import           Control.Concurrent.Async
import           Control.Monad            (forever)
import           Data.ByteString.Char8    (ByteString)
import qualified Data.ByteString.Char8    as BS
import qualified Network.Http.Client      as H
import           Network.URI
import qualified System.IO.Streams        as Streams

veneer :: URI ->  Either String (IO (Async (), ByteString -> IO ByteString))
veneer u = case setupHTTP u of
  Left x -> Left x
  Right f -> Right $ do
    workQueue <- newChan
    worker <- async $ go workQueue f
    return $ (worker, \input -> do
                 mv <- newEmptyMVar
                 writeChan workQueue (input, mv)
                 readMVar mv)

go :: Chan (t, MVar a) -> IO (t -> IO (), IO a) -> IO b
go incoming setup = do
  retChan <- newChan
  (send,recv) <- setup
  sender <- async $ forever $ do
    (input, mv) <- readChan incoming
    () <- send input
    writeChan retChan mv
  forever $ do
    res <- recv
    mv <- readChan retChan
    putMVar mv res


setupHTTP :: URI -> Either String (IO (ByteString -> IO (), IO ByteString))
setupHTTP (URI "http:" (Just (URIAuth "" host port)) path "" "") = Right $ do
  -- hPrint stderr ("charon connecting to", host,port,path)
  c <- H.openConnection (BS.pack host) (read $ dropWhile (==':') port)
  q <- H.buildRequest $ do
    H.http H.POST "/"
    -- H.setAccept "text/json"
  return (send c q,
          receive c)
    where
      send c q msg = H.sendRequest c q $ \o -> do
        Streams.write (Just (Builder.fromByteString $ msg)) o
      receive c = H.receiveResponse c H.concatHandler'
setupHTTP u = Left ("unacceptable URL: " ++ show u)


fromRight :: Either t t1 -> t1
fromRight (Right x) = x
fromRight x = error "fromRight"
