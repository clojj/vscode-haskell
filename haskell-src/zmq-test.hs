{-# LANGUAGE OverloadedStrings #-}
import System.ZMQ4.Monadic

import Control.Concurrent (threadDelay)

import System.Environment (getArgs)
import qualified Data.ByteString.UTF8 as UTF8

main :: IO ()
main = do
  args <- getArgs
  case args of
    [src_filename] -> do
      src <- readFile src_filename
      runZMQ $ do
            s <- socket Req
            bind s "ipc:///tmp/parser"

            -- forever $ do
            lexAndParse s src
            liftIO $ threadDelay (1 * 2000 * 1000)

            lexAndParse s src
            liftIO $ threadDelay (1 * 2000 * 1000)

            send s [] "{{exit}}"
    _ -> error "Usage: zmq-test filename.hs"


lexAndParse :: Socket z Req -> String -> ZMQ z ()
lexAndParse s src = do
    send s [] (UTF8.fromString src)
    a <- receive s
    liftIO $ putStrLn $ UTF8.toString a

