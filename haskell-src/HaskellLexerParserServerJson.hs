{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified GHC
import           GHC.Paths                   (libdir)
import           GHC.SYB.Utils

import           Language.Haskell.GHC.Parser

import           MonadUtils
import           System.Exit

import           Control.Concurrent
import           Control.Concurrent.Chan ()
import           Control.Monad

import           System.ZMQ4.Monadic
import qualified Data.ByteString.UTF8 as UTF8

-- lexing
import           Lexer
import           StringBuffer
import           SrcLoc
import           FastString              (mkFastString)

-- to JSON
import Prelude hiding (mod)

import Data.Aeson (ToJSON(..), object, (.=))
import Data.Data (Data, cast, toConstr, showConstr, gmapQ)
import Data.List (isInfixOf, isPrefixOf, intercalate)
import Data.String (fromString)

import Text.Show.Pretty (Value(..),valToDoc)
import Text.PrettyPrint
import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as B.Lazy
import qualified Data.HashMap.Strict  as HashMap

import Control.Arrow (second)
import Control.Exception

import Bag
import Exception
import GHC
import Module
import Name
import Outputable (Outputable, showSDoc, ppr)
import RdrName
import TcEvidence
import Var
import qualified OccName as Occ

import ErrUtils

import qualified Data.ByteString.UTF8 ()
import qualified Data.ByteString.Lazy          as BSL


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



-- to JSON

{-------------------------------------------------------------------------------
  Translate AST to Value
-------------------------------------------------------------------------------}

pretty :: (Outputable a, GHC.GhcMonad m) => a -> m String
pretty x = do
  dynFlags <- GHC.getSessionDynFlags
  return $! showSDoc dynFlags (ppr x)

pretty' :: (Outputable a, GHC.GhcMonad m) => a -> m Value
pretty' = liftM String . pretty

-- | Construct a `Value` from any term implementing `data`
--
-- We have a number of special cases, solving two different kinds of problems:
--
-- * Some datatypes in GHC don't have an informative Data instance but are
--   marked as "abstract". We test for these types specifically so that we can
--   use a custom pretty-printer rather than just showing "{abstract}".
-- * Some subterms in ghc contain error values. We try to catch these and
--   show them as more readable strings (defaulting to showing the exception).
--
-- Moreover, for a few types we show both the pretty-printed form and the
-- actual tree; we are careful to do this only for top-level occurrences of
-- these types.
valueFromData :: (Data a, GHC.GhcMonad m) => a -> m Value
valueFromData = go False
  where
    -- Bool indicates if we just added a pretty-printed form as well
    -- (so that we don't do it for directly recursive values)
    go :: (Data a, GHC.GhcMonad m) => Bool -> a -> m Value
    go b x
      -- Types where we want to show both a pretty-printed value and a tree
      | Just x' <- cast x :: Maybe (GHC.HsType Name) = withPretty b x'
      | Just x' <- cast x :: Maybe (GHC.HsType Var)  = withPretty b x'
      | Just x' <- cast x :: Maybe GHC.Type          = withPretty b x'
      -- Abstract types we cannot traverse
      | Just x' <- cast x :: Maybe SrcSpan    = pretty' x'
      | Just x' <- cast x :: Maybe GHC.TyCon      = pretty' x'
      -- We cannot traverse names either, but we don't want to just call
      -- the pretty-printer because we would lose too much information
      | Just x' <- cast x :: Maybe GHC.Module     = prettyModule     x'
      | Just x' <- cast x :: Maybe GHC.ModuleName = prettyModuleName x'
      | Just x' <- cast x :: Maybe GHC.Name       = prettyName       x'
      | Just x' <- cast x :: Maybe OccName    = prettyOccName    x'
      | Just x' <- cast x :: Maybe GHC.RdrName    = prettyRdrName    x'
      | Just x' <- cast x :: Maybe TcEvBinds  = prettyTcEvBinds  x'
      | Just x' <- cast x :: Maybe Var        = prettyVar        x'
      -- Otherwise just construct a generic value
      | otherwise = generic False x

    generic :: (Data a, GHC.GhcMonad m) => Bool -> a -> m Value
    generic b x = ghandle handleException $ do
        constrName <- liftIO $ evaluate $ showConstr $ toConstr x
        Con constrName <$> sequence (gmapQ (go b) x)

    withPretty :: (Data a, Outputable a,GHC.GhcMonad m) => Bool -> a -> m Value
    withPretty True  x = generic True x
    withPretty False x = do
        prettied <- pretty x
        tree     <- generic True x
        return $! Rec "" [(prettied, tree)]

    handleException :: GHC.GhcMonad m => SomeException -> m Value
    handleException e =
      case isKnownPanic (show e) of
        Just panic -> return $! String $ "<<" ++ panic ++ ">>"
        Nothing    -> return $! String $ show e

    isKnownPanic :: String -> Maybe String
    isKnownPanic e = msum $ map aux knownPanics
      where
        aux panic | panic `isInfixOf` e = Just panic
                  | otherwise           = Nothing

    knownPanics :: [String]
    knownPanics = [
        "PostTcExpr"
      , "PostTcKind"
      , "PostTcType"
      , "fixity"
      , "placeHolderNames"
      ]

-- | Clean up a value generated by valueFromData
cleanupValue :: Value -> Value
cleanupValue (Con nm vals)
  | nm == "[]"  = case vals of
                    [] -> List []
                    _  -> error "cleanupValue: invalid tree"
  | nm == "(:)" = case vals of
                    [x, xs] -> case cleanupValue xs of
                                 List xs' -> List (cleanupValue x : xs')
                                 _        -> error "cleanupValue: invalid tree"
                    _       -> error "cleanupValue: invalid tree"
  | isTuple nm  = Tuple (map cleanupValue vals)
  | isBag   nm  = case vals of
                    [contents] -> Con "Bag.listToBag" [cleanupValue contents]
                    _          -> error "cleanupValue: invalid tree"
  | otherwise   = Con nm (map cleanupValue vals)
  where
    isTuple :: String -> Bool
    isTuple ('(' : nm') = all (== ',') (init nm') && last nm' == ')'
    isTuple _           = False

    isBag :: String -> Bool
    isBag = isPrefixOf "{abstract:Bag"

cleanupValue (String s)    = String s
cleanupValue (Rec nm flds) = Rec nm $ map (second cleanupValue) flds
cleanupValue _             = error "cleanupValue: unexpected Value"

{-------------------------------------------------------------------------------
  Specialized functions for the different kinds of names

  * OccName  most primitive type: just a string and namespace
               (variable, data constructor, etc.)
  * RdrName  come directly from the parser
  * Name     after renaming
  * Var      after typechecking
  * Id       alias for Var
-------------------------------------------------------------------------------}

prettyOccName :: GHC.GhcMonad m => OccName -> m Value
prettyOccName nm
  | occNameSpace nm == Occ.varName   = mk "VarName"
  | occNameSpace nm == Occ.dataName  = mk "DataName"
  | occNameSpace nm == Occ.tvName    = mk "TvName"
  | occNameSpace nm == Occ.tcClsName = mk "TcClsName"
  | otherwise                        = error "unexpected OccName"
  where
    mk :: GHC.GhcMonad m => String -> m Value
    mk namespace = return $! Rec "" [(namespace, String (occNameString nm))]

prettyTcEvBinds :: GHC.GhcMonad m => TcEvBinds -> m Value
prettyTcEvBinds (TcEvBinds mut) = pretty' mut
prettyTcEvBinds (EvBinds bagOfEvBind) = do
    let evBinds = bagToList bagOfEvBind
    fmap (Con "TcEvBinds") $! mapM prettyEvBind evBinds

prettyEvBind :: GHC.GhcMonad m => EvBind -> m Value
prettyEvBind (EvBind var term) = do
    pVar <- prettyVar var
    pTerm <- pretty' term
    return $! Rec "" [("ev_var", pVar), ("ev_term", pTerm)]

prettyRdrName :: GHC.GhcMonad m => RdrName -> m Value
prettyRdrName (Unqual   nm) = prettyOccName nm
prettyRdrName (Exact    nm) = prettyName nm
prettyRdrName (Qual mod nm) = do
    Rec "" fields <- prettyOccName nm
    qual <- prettyModuleName mod
    return $! Rec "" (("Qual", qual):fields)
prettyRdrName (Orig mod nm) = do
    Rec "" fields <- prettyOccName nm
    orig <- prettyModule mod
    return $! Rec "" (("Orig", orig):fields)

prettyName :: GHC.GhcMonad m => Name -> m Value
prettyName nm = do
    Rec "" fields <- prettyOccName (nameOccName nm)
    loc  <- pretty' (nameSrcSpan nm)
    sort <- prettyNameSort
    return $! Rec "" (("n_loc",loc):("n_sort",sort):fields)
  where
    prettyNameSort :: GHC.GhcMonad m => m Value
    prettyNameSort
      | Just _tyThing <- wiredInNameTyThing_maybe nm = do
          mod <- prettyModule (nameModule nm)
          -- TODO: Do somethng with tyThing
          return $! Rec "" [("WiredIn", mod)]
      | isExternalName nm = do
          mod <- prettyModule (nameModule nm)
          return $! Rec "" [("External", mod)]
      | isInternalName nm = return $! String "Internal"
      | isSystemName nm   = return $! String "System"
      | otherwise         = error "Unexpected NameSort"

prettyVar :: GHC.GhcMonad m => Var -> m Value
prettyVar nm = do
    Rec "" fields <- prettyName $ Var.varName nm
    typ <- valueFromData $ varType nm
    -- TODO: There is more information we could extract about Vars
    return $! Rec "" (("varType", typ):fields)

prettyModuleName :: GHC.GhcMonad m => ModuleName -> m Value
prettyModuleName = return . String . moduleNameString

prettyModule :: GHC.GhcMonad m => Module -> m Value
prettyModule mod = do
    pkg <- prettyPackageKey $ modulePackageKey mod
    nm  <- prettyModuleName $ moduleName       mod
    return $! Con "Module" [pkg, nm]

prettyPackageKey :: GhcMonad m => PackageKey -> m Value
prettyPackageKey = return . String . packageKeyString

{-------------------------------------------------------------------------------
  Extracting ASTs from a set of targets
-------------------------------------------------------------------------------}

data Trees = Trees {
    treeErrors      :: [String]
  , treeModule      :: String
  , treeParsed      :: Value
  , treeRenamed     :: Value
  , treeTypechecked :: Value
  , treeExports     :: Value
  } deriving (Eq,Show)

-- | Convert Trees to Doc
treesToDoc :: Trees -> Doc
treesToDoc Trees{..} =
  text ("# Errors" ++ intercalate "\n" treeErrors)
  $$
  text ("# Module" ++ treeModule)
  $$
  text ""
  $$
  sectionV "## Parsed" treeParsed
  $$
  sectionV "## Renamed" treeRenamed
  $$
  sectionV "## Typechecked" treeTypechecked
  $$
  sectionV "## Exports" treeExports
  where
    sectionV title v = text title $$ valToDoc v $$ text ""

{-------------------------------------------------------------------------------
  Dump the trees to stdout in text format
-------------------------------------------------------------------------------}
dumpText :: [Trees] -> IO ()
dumpText = mapM_ (putStrLn . render . treesToDoc)

{-------------------------------------------------------------------------------
  Dump in JSON format
-------------------------------------------------------------------------------}
instance ToJSON Value where
  -- Special cases
  toJSON (Con "False" []) = Aeson.Bool False
  toJSON (Con "True"  []) = Aeson.Bool True
  toJSON (Con "Bag.listToBag" [xs]) = toJSON xs
  toJSON (Con "L" [loc, x]) =
    case toJSON x of
      Aeson.Object obj' -> Aeson.Object (HashMap.insert "location" (toJSON loc) obj')
      nonObject         -> nonObject -- we lose the location information in this case

  -- Rest
  toJSON (Con nm [])   = Aeson.String (fromString nm)
  toJSON (Con nm vals) = object [ fromString nm .= vals ]
  toJSON (Tuple  vals) = toJSON vals
  toJSON (List   vals) = toJSON vals
  toJSON (String s)    = Aeson.String (fromString s)
  toJSON (Rec "" flds) = object $ map (\(fld, val) -> fromString fld .= val) flds
  toJSON _             = error "toJSON: Unexpected Value"

instance ToJSON Trees where
  toJSON Trees{..} = object [
      "errors"      .= treeErrors
    , "module"      .= treeModule
    , "parsed"      .= treeParsed
    , "renamed"     .= treeRenamed
    , "typechecked" .= treeTypechecked
    , "exports"     .= treeExports
    ]

dumpJson :: [Trees] -> IO ()
dumpJson = B.Lazy.putStr . Aeson.encode
