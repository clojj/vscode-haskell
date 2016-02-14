module Main (main) where

import           Language.Haskell.GHC.DumpTree

import qualified GHC
import           GHC.Paths                     (libdir)
import           MonadUtils
import           System.Directory
import           System.Exit

import           Control.Concurrent
import           Control.Concurrent.Chan       ()
import           Control.Monad                 (forever, when)

import qualified Data.ByteString.UTF8          as UTF8
import           System.ZMQ4.Monadic

import qualified Data.Aeson                    as Aeson
import qualified Data.ByteString.Lazy          as BSL

{- optional lexing
import SrcLoc
import FastString (mkFastString)
import StringBuffer
import Lexer
import ErrUtils
import Outputable

import Data.String.Utils
-}

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
              when (src == "{{exit}}") $ liftIO exitSuccess
              liftIO $ writeChan strChan src
              ast <- liftIO $ readChan strChan
              send s [] ast


worker :: Chan UTF8.ByteString -> IO ()
worker strChan =
    GHC.runGhc (Just libdir) $ forever $ do

            src <- liftIO $ readChan strChan

            -- parsing
            cd <- liftIO getCurrentDirectory
            -- TODO extract (cd ++ "/dummy")
            trees <- treesForTargetsSrc (cd ++ "/dummy") [UTF8.toString src] -- test multiple sources: [UTF8.toString src, "module A }"]
            liftIO $ dumpText trees
            let ast = Aeson.encode trees
            liftIO $ writeChan strChan $ BSL.toStrict ast

            -- optional lexing
{-
            dynFlags <- GHC.getSessionDynFlags
            liftIO $ putStrLn "lexing..."
            let lexLoc = mkRealSrcLoc  (mkFastString "<interactive>") 1 1
            let sb = stringToStringBuffer src_utf8
            let pResult = lexTokenStream sb lexLoc dynFlags
            let lexResult = case pResult of
                      POk _ toks -> concatMap showTokenWithSource (GHC.addSourceToTokens lexLoc sb toks)
                      -- TODO pattern-match instead of startswith
                      PFailed srcspan msg -> "{err}" ++ showSDoc dynFlags (mkLocMessage SevError srcspan msg)

            liftIO $ putStrLn $ "Lexer result:\n" ++ lexResult

            -- TODO pattern-match instead of startswith
            if not $ startswith "{err}" lexResult then do
              -- parsing
              trees <- treesForTargetsSrc [src_utf8]
              let ast = Aeson.encode (lexResult, trees)
              liftIO $ writeChan strChan $ BSL.toStrict ast
            else
              -- TODO check lexer-error JSON
              liftIO $ writeChan strChan $ BSL.toStrict (Aeson.encode lexResult)

-- ITvocurly, ITvccurly = "virtual" braces for layout induced blocks
-- ITocurly, ITccurly = real braces for no-layout blocks
-- ITsemi = blocks(?)

showTokenWithSource :: (GHC.Located Token, String) -> String
showTokenWithSource (loctok, src) =
  "Located Token: " ++ tok ++ "\n" ++
  "Source: " ++ src ++ "\n" ++
  "Location: " ++ srcloc ++
  "\n\n" where
    tok = show $ unLoc loctok
    srcloc = show $ getLoc loctok

showToken :: GenLocated SrcSpan Token -> String
showToken t = "srcLoc: " ++ "\n" ++ srcloc ++ "\ntok: " ++ tok where
  srcloc = show $ getLoc t
  tok = show $ unLoc t
-}
