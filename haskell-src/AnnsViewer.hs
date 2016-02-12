module Main where

import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Parsers
import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint.Utils
import Data.Map
import System.IO
import Control.Monad
import GHC.SYB.Utils

main :: IO ()
main = do
  parseFile
  Right (as, m) <- parseModule "testing/AnnsSample.hs"
  printAnns as
  putStrLn "ParsedSource:"
  putStrLn $ showData Parser 1 m


-- type Anns = Map.Map AnnKey Annotation

printAnns :: Anns -> IO ()
printAnns anns = do
  let lst =  toList anns
  foldM_ printAnn () lst
  where printAnn _  (key, ann) = do
          putStr "Key: "
          putStrLn (show key)
          putStr "Ann: "
          putStrLn (show ann)
          putStrLn "================================="
