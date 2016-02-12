module Main where

import           ErrUtils                (mkPlainErrMsg)
import           FastString              (mkFastString)
import           GHC
import           GHC.Paths               (libdir)
import           Lexer
import qualified MonadUtils              as GMU
import           SrcLoc
import           StringBuffer

import           Control.Concurrent
import           Control.Concurrent.Chan ()
import           Control.Monad           (forever, when)

import System.Exit

import System.IO
import System.ZMQ4.Monadic
import qualified Data.ByteString.Char8 as CS

-- ITvocurly ("virtual" braces for layout induced blocks)
-- ITocurly (real braces for no-layout blocks)
-- same for ITvccurly

main :: IO ()
main = do
   strChan <- newChan
   threadId <- forkIO $ worker strChan
   print $ show threadId
   runZMQ $ do
          s <- socket Rep
          connect s "ipc:///tmp/lexer"
          forever $ do
              src <- receive s
              let src' = CS.unpack src
              liftIO $ when (src' == "{{exit}}") exitSuccess
              -- liftIO $ CS.putStrLn src
              liftIO $ writeChan strChan src'
              send s [] (CS.pack "ack") -- TODO send response/positions/tokens
              liftIO $ hFlush stdout

worker :: Chan String -> IO ()
worker strChan =
    runGhc (Just libdir) $ do
            flags <- getSessionDynFlags
            let lexLoc = mkRealSrcLoc  (mkFastString "<interactive>") 1 1

            GMU.liftIO $ forever $ do
              str <- readChan strChan
              let sb = stringToStringBuffer str
              let pResult = lexTokenStream sb lexLoc flags
              case pResult of
                      -- POk _ toks    -> GMU.liftIO $ putStr $ concatMap showToken toks
                      POk _ toks -> GMU.liftIO $ putStr $ concatMap showTokenWithSource (addSourceToTokens lexLoc sb toks)
                      PFailed srcspan msg -> do
                        GMU.liftIO $ print $ show srcspan
                        GMU.liftIO $ do
                          putStrLn "Lexer Error:"
                          print $ mkPlainErrMsg flags srcspan msg

showToken :: GenLocated SrcSpan Token -> String
showToken t = "srcLoc: " ++ "\n" ++ srcloc ++ "\ntok: " ++ tok where
  srcloc = show $ getLoc t
  tok = show $ unLoc t

showTokenWithSource :: (Located Token, String) -> String
showTokenWithSource (loctok, src) =
  "Located Token: " ++ tok ++ "\n" ++
  "Source: " ++ src ++ "\n" ++
  "Location: " ++ srcloc ++
  "\n\n" where
    tok = show $ unLoc loctok
    srcloc = show $ getLoc loctok
