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
import           SrcLoc
import           FastString                  (mkFastString)
import           Lexer
import           StringBuffer

import qualified Data.Aeson                  as Aeson

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
                              POk _ toks -> return $ map showToken toks
                              -- POk _ toks -> return $ map showTokenWithSource (GHC.addSourceToTokens lexLoc sb toks)
                              PFailed srcspan msg -> do
                                let lexerError = mkPlainErrMsg dynflags srcspan msg
                                liftIO $ print $ show srcspan
                                liftIO $ do
                                  putStrLn "Lexer Error:"
                                  print lexerError
                                return [(show lexerError, (0::Int,0::Int), (0::Int,0::Int))]
                  -- TODO only parse, if there are no lexer errors ?

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
                      let treeEmpty = String ""
                      let trees = Trees{treeErrors = [err, show sloc],
                                  treeModule = "",
                                  treeParsed = treeEmpty,
                                  treeRenamed = treeEmpty,
                                  treeTypechecked = treeEmpty,
                                  treeExports = treeEmpty}
                      let ts = Aeson.encode trees
                      liftIO $ writeChan channel $ UTF8.fromString $ "LEXING\n" ++ show lexResult ++ "\nPARSING\n" ++ UTF8.toString (BSL.toStrict ts) ++ "\n"

                    Parsed s -> do
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

                      liftIO $ writeChan channel $ UTF8.fromString $ "LEXING\n" ++ show lexResult ++ "\nPARSING\n" ++ UTF8.toString (BSL.toStrict ast) ++ "\n"

  where
    mkTree :: (Data a, GHC.GhcMonad m) => a -> m Value
    mkTree = liftM cleanupValue . valueFromData

-- output of lexer

-- ITvocurly, ITvccurly = "virtual" braces for layout induced blocks
-- ITocurly, ITccurly = real braces for no-layout blocks
-- ITsemi = blocks(?)

getGhcLoc :: GHC.SrcSpan -> (Int, Int)
getGhcLoc (GHC.RealSrcSpan ss)  = (GHC.srcSpanStartLine ss, GHC.srcSpanStartCol ss)
getGhcLoc (GHC.UnhelpfulSpan _) = (-1,-1)

-- | gets the (row,col) of the end of the @GHC.SrcSpan@, or (-1,-1)
-- if there is an @GHC.UnhelpfulSpan@
getGhcLocEnd :: GHC.SrcSpan -> (Int, Int)
getGhcLocEnd (GHC.RealSrcSpan ss)  = (GHC.srcSpanEndLine ss, GHC.srcSpanEndCol ss)
getGhcLocEnd (GHC.UnhelpfulSpan _) = (-1,-1)

showToken :: SrcLoc.Located Token -> (String, (Int, Int), (Int, Int))
-- showToken t = show location ++ "\n" ++ tok ++ "\n" where
showToken t = (tok, start, end) where
  srcSpan = getLoc t
  start = getGhcLoc srcSpan
  end = getGhcLocEnd srcSpan
  tok = show $ GHC.unLoc t

showTokenWithSource :: (GHC.Located Token, String) -> String
showTokenWithSource (loctok, src) =
  "Located Token: " ++ tok ++ "\n" ++
  "Source: " ++ src ++ "\n" ++
  "Location: " ++ srcloc ++
  "\n\n" where
    tok = show $ GHC.unLoc loctok
    srcloc = show $ GHC.getLoc loctok

