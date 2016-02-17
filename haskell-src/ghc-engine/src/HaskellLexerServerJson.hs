module Main where

import Util

import qualified GHC
import           GHC.Paths                   (libdir)
import           System.Exit

import           Control.Monad

import qualified Data.ByteString.UTF8        as UTF8
import qualified System.ZMQ4.Monadic         as ZMQ

-- lexing
import           SrcLoc
import           FastString                  (mkFastString)
import           Lexer
import           StringBuffer

import qualified Data.Aeson                  as Aeson

import           ErrUtils

import qualified Data.ByteString.Lazy        as BSL
import qualified Data.ByteString.UTF8        ()

main :: IO ()
main = ZMQ.runZMQ $ do
          s <- ZMQ.socket ZMQ.Rep
          ZMQ.connect s "ipc:///tmp/lexer"
          dynflags <- ZMQ.liftIO $ GHC.runGhc (Just libdir) GHC.getSessionDynFlags

          forever $ do
              src <- ZMQ.receive s
              let src' = UTF8.toString src
              ZMQ.liftIO $ when (src' == "{{exit}}") exitSuccess
              let lexLoc = mkRealSrcLoc  (mkFastString "<interactive>") 1 1
              let sb = stringToStringBuffer (UTF8.toString src)
              let lexed = lexTokenStream sb lexLoc dynflags
              lexResult <- case lexed of
                          -- map showTokenWithSource (GHC.addSourceToTokens lexLoc sb toks)
                          POk _ toks -> return $ Right (map showToken toks)
                          PFailed srcSpan msg -> do
                            let lexerError = mkPlainErrMsg dynflags srcSpan msg
                            return $ Left (show lexerError, getPos srcSpan)

              let json = Aeson.encode lexResult
              ZMQ.send s [] $ UTF8.fromString $ UTF8.toString (BSL.toStrict json) ++ "\n"
