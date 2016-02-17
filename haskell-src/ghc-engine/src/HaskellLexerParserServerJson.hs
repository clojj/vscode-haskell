module Main where

import           Util
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

import Data.Aeson (ToJSON(..), object, (.=))

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
                  let lexed = lexTokenStream sb lexLoc dynflags
                  lexResult <- case lexed of
                              -- map showTokenWithSource (GHC.addSourceToTokens lexLoc sb toks)
                              POk _ toks -> return $ Right (map showToken toks)
                              PFailed srcSpan msg -> do
                                let lexerError = mkPlainErrMsg dynflags srcSpan msg
                                return $ Left (show lexerError, getPos srcSpan)

                  -- only parse, if there are no lexer errors !
                  case lexResult of
                    Left _  -> liftIO $ writeJsonLexer lexResult
                    Right _ -> do
                      -- parsing
                      let out = runParser dynflags parserModule (UTF8.toString src)
                      case out of

                        -- TODO remove Partial for GHC >= 7.10 ?
                        Partial s (s1, s2) -> do
                          liftIO $ putStrLn "Partial:"
                          liftIO $ putStrLn $ showData Parser 2 s
                          liftIO $ putStrLn $ showData Parser 2 s1
                          liftIO $ putStrLn $ showData Parser 2 s2

                        Failure err (Loc line column) -> do
                          let parseResult = Left (err, (line, column))
                          liftIO $ writeJson lexResult parseResult

                        Parsed s -> do
                          treeParsed <- mkTree s
                          let parseResult = Right treeParsed
                          liftIO $ writeJson lexResult parseResult

  where
    mkTree :: (Data a, GHC.GhcMonad m) => a -> m Value
    mkTree = liftM cleanupValue . valueFromData

    writeJsonLexer :: Either LexerError [TokenWithPos] -> IO ()
    writeJsonLexer lexResult = do
      let json = Aeson.encode Result {lexing = lexResult, parsing = Nothing}
      writeChan channel $ UTF8.fromString $ UTF8.toString (BSL.toStrict json) ++ "\n"

    writeJson :: Either LexerError [TokenWithPos] -> Either (String, (LineNumber, ColumnNumber)) Value -> IO ()
    writeJson lexResult trees = do
      let json = Aeson.encode Result {lexing = lexResult, parsing = Just trees}
      writeChan channel $ UTF8.fromString $ UTF8.toString (BSL.toStrict json) ++ "\n"


data Result = Result {
    lexing      :: Either LexerError [TokenWithPos]
  , parsing     :: Maybe (Either (String, (LineNumber, ColumnNumber)) Value)
  } deriving (Eq,Show)

instance ToJSON Result where
  toJSON Result{..} = object [
      "lexing"      .= lexing
    , "parsing"     .= parsing
    ]
