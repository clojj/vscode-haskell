module Main where

import qualified GHC
import           GHC.Paths                   (libdir)
import           GHC.SYB.Utils

import           Language.Haskell.GHC.Parser

import           MonadUtils
import           System.Exit

import           Control.Concurrent
import           Control.Concurrent.Chan ()
import           Control.Monad           (forever, when)

import           System.ZMQ4.Monadic
import qualified Data.ByteString.UTF8 as UTF8


main :: IO ()
main = do
   strChan <- newChan
   threadId <- forkIO $ worker strChan
   print $ show threadId
   runZMQ $ do
          s <- socket Rep
          connect s "ipc:///tmp/parser"
          forever $ do
              src <- receive s
              let src' = UTF8.toString src
              liftIO $ when (src' == "{{exit}}") exitSuccess
              liftIO $ writeChan strChan src'
              ast <- liftIO $ readChan strChan
              send s [] (UTF8.fromString ast) -- TODO send response/positions/tokens

worker :: Chan String -> IO ()
worker strChan =
    GHC.runGhc (Just libdir) $ do
          dflags <- GHC.getSessionDynFlags
          liftIO $ forever $ do
                  src <- readChan strChan
                  let out = runParser dflags parserModule src
                  case out of
                    -- todo: remove Partial for GHC >= 7.10 ?
                    Partial s (s1, s2) -> do
                      liftIO $ putStrLn "Partial:"
                      liftIO $ putStrLn $ showData Parser 2 s
                      liftIO $ putStrLn $ showData Parser 2 s1
                      liftIO $ putStrLn $ showData Parser 2 s2
                    Failure err sloc -> do
                      liftIO $ print err
                      liftIO $ print sloc
                      writeChan strChan $ err ++ " " ++ show sloc
                    Parsed s -> do
                      let ast = showData Parser 2 s
                      liftIO $ putStrLn ast
                      writeChan strChan ast
