module Main where

import           Jsonize

import qualified GHC
import           GHC.Paths                   (libdir)
import           GHC.SYB.Utils

import           Language.Haskell.GHC.Parser

import           MonadUtils
import           System.Exit

import           Control.Concurrent
import           Control.Concurrent.Chan     ()
import           Control.Monad

import qualified Data.ByteString.UTF8        as UTF8
import           System.ZMQ4.Monadic

-- lexing
import           FastString                  (mkFastString)
import           Lexer
import           SrcLoc
import           StringBuffer

import qualified Data.Aeson                  as Aeson
import           GHC

import           ErrUtils

import qualified Data.ByteString.Lazy        as BSL
import qualified Data.ByteString.UTF8        ()

import           Data.Data                   (Data)
import           Text.Show.Pretty            (Value (..))


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
              liftIO $ writeChan channel src

              result <- liftIO $ readChan channel
              send s [] result

worker :: Chan UTF8.ByteString -> IO ()
worker channel =
    GHC.runGhc (Just libdir) $ do
          dynflags <- GHC.getSessionDynFlags
          forever $ do

                  src <- liftIO $ readChan channel

                  -- lexing
                  let lexLoc = mkRealSrcLoc  (mkFastString "<interactive>") 1 1
                  let sb = stringToStringBuffer (UTF8.toString src)
                  let pResult = lexTokenStream sb lexLoc dynflags
                  lexResult <- case pResult of
                              -- POk _ toks    -> liftIO $ putStr $ concatMap showToken toks
                              POk _ toks -> return $ map showTokenWithSource (GHC.addSourceToTokens lexLoc sb toks)
                              PFailed srcspan msg -> do
                                let lexerError = mkPlainErrMsg dynflags srcspan msg
                                liftIO $ print $ show srcspan
                                liftIO $ do
                                  putStrLn "Lexer Error:"
                                  print lexerError
                                return [show lexerError]

                  -- parsing
                  let out = runParser dynflags parserModule (UTF8.toString src)
                  case out of
                    -- TODO remove Partial for GHC >= 7.10 ?
                    Partial s (s1, s2) -> do
                      liftIO $ putStrLn "Partial:"
                      liftIO $ putStrLn $ showData Parser 2 s
                      liftIO $ putStrLn $ showData Parser 2 s1
                      liftIO $ putStrLn $ showData Parser 2 s2
                    Failure err sloc -> do
                      liftIO $ print err
                      liftIO $ print sloc
                      -- TODO errors as JSON !
                      liftIO $ writeChan channel $ UTF8.fromString $ "LEXING\n" ++ concat lexResult ++ "\nPARSING\n" ++ err ++ " " ++ show sloc
                    Parsed s -> do
                      -- TODO emit ast as JSON !
                      treeParsed <- mkTree s
                      let treeEmpty = String ""
                      let trees = Trees{treeErrors = [],
                                  treeModule = "",
                                  treeParsed = treeParsed,
                                  treeRenamed = treeEmpty,
                                  treeTypechecked = treeEmpty,
                                  treeExports = treeEmpty}

                      let ast = Aeson.encode (lexResult, trees)

                      -- let ast = showData Parser 2 s
                      -- liftIO $ putStrLn ast

                      liftIO $ writeChan channel $ UTF8.fromString ("LEXING\n" ++ concat lexResult ++ "\nPARSING\n" ++ UTF8.toString (BSL.toStrict ast) ++ "\n")

  where
    mkTree :: (Data a,GhcMonad m) => a -> m Value
    mkTree = liftM cleanupValue . valueFromData

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

