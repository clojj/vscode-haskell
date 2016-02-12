{-# LANGUAGE CPP #-}
module Language.Haskell.GHC.DumpTree
  ( treesForTargetsSrc
  , treesForSession
  , treeDumpFlags
  , dumpJson
  , treesToDoc
  , dumpText
  , Trees(..)
  ) where

import Prelude hiding (mod)
import Control.Arrow (second)
import Control.Exception
import Control.Monad
import Data.Aeson (ToJSON(..), object, (.=))
import Data.Data (Data, cast, toConstr, showConstr, gmapQ)
import Data.List (isInfixOf, isPrefixOf)
import Data.String (fromString)

import Text.Show.Pretty (Value(..),valToDoc)
import Text.PrettyPrint
import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as B.Lazy
import qualified Data.HashMap.Strict  as HashMap

import Bag
import Exception
import GHC
import HscTypes
import Module
import MonadUtils
import Name
import Outputable (Outputable, showSDoc, ppr)
import RdrName
import TcEvidence
import Var
import qualified OccName as Occ

import SrcLoc
import FastString (mkFastString)

import StringBuffer
import Data.Time.Clock.POSIX

import Lexer
import ErrUtils

{-------------------------------------------------------------------------------
  Translate AST to Value
-------------------------------------------------------------------------------}

pretty :: (Outputable a, GhcMonad m) => a -> m String
pretty x = do
#if MIN_VERSION_ghc(7,6,3)
  dynFlags <- getSessionDynFlags
  return $! showSDoc dynFlags (ppr x)
#else
  return $! showSDoc (ppr x)
#endif

pretty' :: (Outputable a, GhcMonad m) => a -> m Value
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
valueFromData :: (Data a, GhcMonad m) => a -> m Value
valueFromData = go False
  where
    -- Bool indicates if we just added a pretty-printed form as well
    -- (so that we don't do it for directly recursive values)
    go :: (Data a, GhcMonad m) => Bool -> a -> m Value
    go b x
      -- Types where we want to show both a pretty-printed value and a tree
      | Just x' <- cast x :: Maybe (HsType Name) = withPretty b x'
      | Just x' <- cast x :: Maybe (HsType Var)  = withPretty b x'
      | Just x' <- cast x :: Maybe Type          = withPretty b x'
      -- Abstract types we cannot traverse
      | Just x' <- cast x :: Maybe SrcSpan    = pretty' x'
      | Just x' <- cast x :: Maybe TyCon      = pretty' x'
      -- We cannot traverse names either, but we don't want to just call
      -- the pretty-printer because we would lose too much information
      | Just x' <- cast x :: Maybe Module     = prettyModule     x'
      | Just x' <- cast x :: Maybe ModuleName = prettyModuleName x'
      | Just x' <- cast x :: Maybe Name       = prettyName       x'
      | Just x' <- cast x :: Maybe OccName    = prettyOccName    x'
      | Just x' <- cast x :: Maybe RdrName    = prettyRdrName    x'
      | Just x' <- cast x :: Maybe TcEvBinds  = prettyTcEvBinds  x'
      | Just x' <- cast x :: Maybe Var        = prettyVar        x'
      -- Otherwise just construct a generic value
      | otherwise = generic False x

    generic :: (Data a, GhcMonad m) => Bool -> a -> m Value
    generic b x = ghandle handleException $ do
        constrName <- liftIO $ evaluate $ showConstr $ toConstr x
        Con constrName <$> sequence (gmapQ (go b) x)

    withPretty :: (Data a, Outputable a,GhcMonad m) => Bool -> a -> m Value
    withPretty True  x = generic True x
    withPretty False x = do
        prettied <- pretty x
        tree     <- generic True x
        return $! Rec "" [(prettied, tree)]

    handleException :: GhcMonad m => SomeException -> m Value
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

prettyOccName :: GhcMonad m => OccName -> m Value
prettyOccName nm
  | occNameSpace nm == Occ.varName   = mk "VarName"
  | occNameSpace nm == Occ.dataName  = mk "DataName"
  | occNameSpace nm == Occ.tvName    = mk "TvName"
  | occNameSpace nm == Occ.tcClsName = mk "TcClsName"
  | otherwise                        = error "unexpected OccName"
  where
    mk :: GhcMonad m => String -> m Value
    mk namespace = return $! Rec "" [(namespace, String (occNameString nm))]

prettyTcEvBinds :: GhcMonad m => TcEvBinds -> m Value
prettyTcEvBinds (TcEvBinds mut) = pretty' mut
prettyTcEvBinds (EvBinds bagOfEvBind) = do
    let evBinds = bagToList bagOfEvBind
    fmap (Con "TcEvBinds") $! mapM prettyEvBind evBinds

prettyEvBind :: GhcMonad m => EvBind -> m Value
prettyEvBind (EvBind var term) = do
    pVar <- prettyVar var
    pTerm <- pretty' term
    return $! Rec "" [("ev_var", pVar), ("ev_term", pTerm)]

prettyRdrName :: GhcMonad m => RdrName -> m Value
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

prettyName :: GhcMonad m => Name -> m Value
prettyName nm = do
    Rec "" fields <- prettyOccName (nameOccName nm)
    loc  <- pretty' (nameSrcSpan nm)
    sort <- prettyNameSort
    return $! Rec "" (("n_loc",loc):("n_sort",sort):fields)
  where
    prettyNameSort :: GhcMonad m => m Value
    prettyNameSort
      | Just _tyThing <- wiredInNameTyThing_maybe nm = do
          mod <- prettyModule (nameModule nm)
          -- TODO: Do somethng with tyThing
          return $! Rec "" [("WiredIn", mod)]
      | isExternalName nm = do
          mod <- prettyModule (nameModule nm)
          return $! Rec "" [("External", mod)]
      | isInternalName nm = do
          return $! String "Internal"
      | isSystemName nm = do
          return $! String "System"
      | otherwise =
          error "Unexpected NameSort"

prettyVar :: GhcMonad m => Var -> m Value
prettyVar nm = do
    Rec "" fields <- prettyName $ Var.varName nm
    typ <- valueFromData $ varType nm
    -- TODO: There is more information we could extract about Vars
    return $! Rec "" (("varType", typ):fields)

prettyModuleName :: GhcMonad m => ModuleName -> m Value
prettyModuleName = return . String . moduleNameString

#if MIN_VERSION_ghc(7,10,0)
prettyModule :: GhcMonad m => Module -> m Value
prettyModule mod = do
    pkg <- prettyPackageKey $ modulePackageKey mod
    nm  <- prettyModuleName $ moduleName       mod
    return $! Con "Module" [pkg, nm]

prettyPackageKey :: GhcMonad m => PackageKey -> m Value
prettyPackageKey = return . String . packageKeyString
#else
prettyModule :: GhcMonad m => Module -> m Value
prettyModule mod = do
    pkg <- prettyPackageId  $ modulePackageId mod
    nm  <- prettyModuleName $ moduleName      mod
    return $! Con "Module" [pkg, nm]

prettyPackageId :: GhcMonad m => PackageId -> m Value
prettyPackageId = return . String . packageIdString
#endif

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

errBagToStrList :: Bag ErrMsg -> [String]
errBagToStrList = map show . reverse . bagToList

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

treesForModSummary :: GhcMonad m => String -> DynFlags -> ModSummary -> m Trees
treesForModSummary src dynFlags modSummary = do

  -- lexing
   let lexLoc = mkRealSrcLoc  (mkFastString "<interactive>") 1 1
   let sb = stringToStringBuffer src
   let pResult = lexTokenStream sb lexLoc dynFlags
   lexResult <- case pResult of
              -- POk _ toks    -> liftIO $ putStr $ concatMap showToken toks
              POk _ toks -> return $ concatMap showTokenWithSource (GHC.addSourceToTokens lexLoc sb toks)
              PFailed srcspan msg -> do
                let lexerError = mkPlainErrMsg dynFlags srcspan msg
                liftIO $ print $ show srcspan
                liftIO $ do
                  putStrLn "Lexer Error:"
                  print lexerError
                return $ show lexerError

   liftIO $ putStrLn lexResult

   --  let wrapErr se = return $ Left $ show $ errBagToStrList $ srcErrorMessages se
   --  let wrapErr se = return $ Left $ show $ bagToList $ mapBag errMsgSpan $ srcErrorMessages se

   -- todo: return easy JSON-format
   let wrapErr se = return $ Left result where
                              sem = srcErrorMessages se
                              msgs = errBagToStrList sem
                              result = msgs ++ (bagToList $ mapBag (show . errMsgSpan) sem)

   eParsed <- handleSourceError wrapErr (Right <$> parseModule modSummary)

   ts <- case eParsed of
           Right parsed -> do
             let wrapErr' se = return $ Left $ show $ bagToList $ srcErrorMessages se
             eTypechecked <- handleSourceError wrapErr' (Right <$> typecheckModule parsed)

             treeModule      <- pretty (ms_mod_name modSummary)
             treeParsed      <- mkTree (pm_parsed_source parsed)
             treeRenamed     <- mkRenamedTree     eTypechecked
             treeTypechecked <- mkTypeCheckedTree eTypechecked
             treeExports     <- mkExportTree      eTypechecked
             let treeErrors = []
             return Trees{..}

           Left errors -> do
             --  liftIO $ putStrLn $ "ERRORS: " ++ concat errors

             -- todo: better format ?
             let t1 = String ""
             return Trees{treeErrors = errors,
                          treeModule = "",
                          treeParsed = t1,
                          treeRenamed = t1,
                          treeTypechecked = t1,
                          treeExports = t1}
   return ts

  where
    mkTree :: (Data a,GhcMonad m) => a -> m Value
    mkTree = liftM cleanupValue . valueFromData

    mkRenamedTree (Right typechecked) =
      case tm_renamed_source typechecked of
         Just renamed -> mkTree renamed
         Nothing      -> return $ String $ show treeNotAvailable
    mkRenamedTree (Left errors) = return (String errors)

    mkTypeCheckedTree (Right typechecked) =
      mkTree $ tm_typechecked_source typechecked
    mkTypeCheckedTree (Left errors) = return (String errors)

    mkExportTree (Right typechecked) =
      mkTree $ modInfoExports $ tm_checked_module_info typechecked
    mkExportTree (Left _) = return $ String $ show treeNotAvailable

    treeNotAvailable :: String
    treeNotAvailable = "<<NOT AVAILABLE>>"

-- | Get dyn flags: Don't compile anything
treeDumpFlags :: DynFlags -> DynFlags
treeDumpFlags dynFlags = dynFlags {
        hscTarget = HscNothing
      , ghcLink   = NoLink
      }

-- | Generate trees for modules in session
treesForSession :: GhcMonad m => String -> DynFlags -> m [Trees]
treesForSession src dynFlags = do
  hscEnv <- getSession
  mapM (treesForModSummary src dynFlags) $ hsc_mod_graph hscEnv

-- | Generate trees for given sources, when already in GHC
treesForTargetsSrc :: GhcMonad m => [String] -> m [Trees]
treesForTargetsSrc (src : srcs) = do
  liftIO $ putStrLn "in treesForTargets"
  liftIO $ putStrLn src
  gbracket
    getSessionDynFlags
    setSessionDynFlags
    $ \dynFlags -> do
      let dynFlags' = treeDumpFlags dynFlags
      void $ setSessionDynFlags dynFlags'
      -- Construct module graph
      setTargets (map mkTarget (src : srcs))
      void $ load LoadAllTargets
      --
      -- generate each module
      treesForSession src dynFlags'
  where
    mkTarget :: String -> Target
    mkTarget s = Target {
        -- todo: find better TargetFile
        targetId           = TargetFile "/Users/jwin/VSCodeExtensions/vscode-extension-samples/decorator-sample/haskell-src/data/HelloWorld2.hs" Nothing
      , targetAllowObjCode = False
      , targetContents     = Just (stringToStringBuffer s, posixSecondsToUTCTime $ fromIntegral (10 :: Integer))
      }


-- | Convert Trees to Doc
treesToDoc :: Trees -> Doc
treesToDoc Trees{..} = do
  text ("# " ++ treeModule)
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
  -- where
  --   go :: Trees -> IO ()
  --   go Trees{..} = do
  --     section ("# " ++ treesModule) $ do
  --       section "## Parsed"      $ showTree treeParsed
  --       section "## Renamed"     $ showTree treeRenamed
  --       section "## Typechecked" $ showTree treeTypechecked
  --       section "## Exports"     $ showTree treeExports
  --
  --   section :: String -> IO () -> IO ()
  --   section title = bracket_ (putStrLn title) (putStrLn "")
  --
  --   showTree :: Value -> IO ()
  --   showTree = putStrLn . valToStr

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

{-------------------------------------------------------------------------------
  Orphans
-------------------------------------------------------------------------------}

#if MIN_VERSION_ghc(7,8,0)
#else
instance Applicative Ghc where
  pure  = return
  (<*>) = ap
#endif
