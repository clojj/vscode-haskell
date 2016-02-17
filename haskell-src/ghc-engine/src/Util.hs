module Util where

import           Lexer
import qualified GHC
import           SrcLoc

-- output of lexer

-- ITvocurly, ITvccurly = "virtual" braces for layout induced blocks
-- ITocurly, ITccurly = real braces for no-layout blocks
-- ITsemi = blocks(?)
type TokenWithPos = (String, Position)
type LexerError = (String, Position)
type Position = (Int, Int, Int, Int)

showToken :: SrcLoc.Located Token -> (String, Position)
-- showToken t = show location ++ "\n" ++ tok ++ "\n" where
showToken t = (tok, pos) where
  srcSpan = getLoc t
  pos = getPos srcSpan
  tok = show $ GHC.unLoc t

-- fix offset (0-based) for vscode
getPos :: GHC.SrcSpan -> Position
getPos srcSpan = (startLine-1, startColumn-1, endLine-1, endColumn-1) where
  (startLine, startColumn) = getGhcLoc srcSpan
  (endLine, endColumn) = getGhcLocEnd srcSpan

getGhcLoc :: GHC.SrcSpan -> (Int, Int)
getGhcLoc (GHC.RealSrcSpan ss)  = (GHC.srcSpanStartLine ss, GHC.srcSpanStartCol ss)
getGhcLoc (GHC.UnhelpfulSpan _) = (-1,-1)

-- | gets the (row,col) of the end of the @GHC.SrcSpan@, or (-1,-1)
-- if there is an @GHC.UnhelpfulSpan@
getGhcLocEnd :: GHC.SrcSpan -> (Int, Int)
getGhcLocEnd (GHC.RealSrcSpan ss)  = (GHC.srcSpanEndLine ss, GHC.srcSpanEndCol ss)
getGhcLocEnd (GHC.UnhelpfulSpan _) = (-1,-1)

showTokenWithSource :: (GHC.Located Token, String) -> String
showTokenWithSource (loctok, src) =
  "Located Token: " ++ tok ++ "\n" ++
  "Source: " ++ src ++ "\n" ++
  "Location: " ++ srcloc ++
  "\n\n" where
    tok = show $ GHC.unLoc loctok
    srcloc = show $ GHC.getLoc loctok
