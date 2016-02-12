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

-- lexing
import           Lexer
import           StringBuffer
import           SrcLoc
import           ErrUtils                (mkPlainErrMsg)
import           FastString              (mkFastString)


main :: IO ()
main = do
   channel <- newChan
   threadId <- forkIO $ worker channel
   print $ show threadId
   runZMQ $ do
          s <- socket Rep
          connect s "ipc:///tmp/parser"
          forever $ do
              src <- receive s
              let src' = UTF8.toString src
              liftIO $ when (src' == "{{exit}}") exitSuccess
              liftIO $ writeChan channel src'

              result <- liftIO $ readChan channel
              send s [] $ UTF8.fromString result

worker :: Chan String -> IO ()
worker channel =
    GHC.runGhc (Just libdir) $ do
          dynflags <- GHC.getSessionDynFlags
          liftIO $ forever $ do

                  src <- readChan channel

                  -- lexing
                  let lexLoc = mkRealSrcLoc  (mkFastString "<interactive>") 1 1
                  let sb = stringToStringBuffer src
                  let pResult = lexTokenStream sb lexLoc dynflags
                  lexResult <- case pResult of
                              -- POk _ toks    -> liftIO $ putStr $ concatMap showToken toks
                              POk _ toks -> return $ concatMap showTokenWithSource (GHC.addSourceToTokens lexLoc sb toks)
                              PFailed srcspan msg -> do
                                let lexerError = mkPlainErrMsg dynflags srcspan msg
                                liftIO $ print $ show srcspan
                                liftIO $ do
                                  putStrLn "Lexer Error:"
                                  print lexerError
                                return $ show lexerError

                  -- parsing
                  let out = runParser dynflags parserModule src
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
                      writeChan channel $ "LEXING\n" ++ lexResult ++ "\nPARSING\n" ++ err ++ " " ++ show sloc
                    Parsed s -> do
                      let ast = showData Parser 2 s
                      -- liftIO $ putStrLn ast
                      writeChan channel ("LEXING\n" ++ lexResult ++ "\nPARSING\n" ++ ast ++ "\n")

-- output of lexer

-- ITvocurly, ITvccurly = "virtual" braces for layout induced blocks
-- ITocurly, ITccurly = real braces for no-layout blocks
-- ITsemi = blocks(?)

showToken :: GenLocated SrcSpan Token -> String
showToken t = "srcLoc: " ++ "\n" ++ srcloc ++ "\ntok: " ++ tok where
  srcloc = show $ getLoc t
  tok = show $ unLoc t

showTokenWithSource :: (GHC.Located Token, String) -> String
showTokenWithSource (loctok, src) =
  "Located Token: " ++ tok ++ "\n" ++
  "Source: " ++ src ++ "\n" ++
  "Location: " ++ srcloc ++
  "\n\n" where
    tok = show $ unLoc loctok
    srcloc = show $ getLoc loctok
