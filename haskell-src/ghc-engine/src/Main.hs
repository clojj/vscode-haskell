{-# LANGUAGE CPP #-}
module Main (main) where

import Language.Haskell.GHC.DumpTree

import qualified GHC
import           GHC.Paths                   (libdir)
import           MonadUtils
import           System.Exit

import           Control.Concurrent
import           Control.Concurrent.Chan ()
import           Control.Monad           (forever, when)

import           System.ZMQ4.Monadic
import qualified Data.ByteString.UTF8 as UTF8

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as BSL

#if MIN_VERSION_base(4,8,0)
#else
import Data.Monoid (mconcat)
#endif

{-------------------------------------------------------------------------------
  Main application
-------------------------------------------------------------------------------}

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
              when (src == "{{exit}}") $
                liftIO exitSuccess

              liftIO $ writeChan strChan src
              ast <- liftIO $ readChan strChan
              send s [] ast -- TODO send response/positions/tokens etc.



worker :: Chan UTF8.ByteString -> IO ()
worker strChan =
    GHC.runGhc (Just libdir) $ do
          forever $ do
            src <- liftIO $ readChan strChan
            let src_utf8 = UTF8.toString src
            trees <- treesForTargetsSrc [src_utf8]
            let ast = Aeson.encode trees
            liftIO $ writeChan strChan $ BSL.toStrict ast
