module Language.Haskell.GHC.DumpTree
  ( treesForTargetsSrc
  , treesForSession
  , treeDumpFlags
  , Trees(..)
  ) where

import Jsonize

import Prelude hiding (mod)
import Control.Monad

import Bag
import Exception
import GHC
import HscTypes
import MonadUtils

import StringBuffer
import Data.Time.Clock.POSIX
import ErrUtils

import           Data.Data                   (Data)
import           Data.List                   (intercalate)
import           Text.Show.Pretty            (Value (..))


treesForModSummary :: GhcMonad m => ModSummary -> m Trees
treesForModSummary modSummary = do
   liftIO $ putStrLn "in treesForModSummary"

   --  let wrapErr se = return $ Left $ show $ errBagToStrList $ srcErrorMessages se
   --  let wrapErr se = return $ Left $ show $ bagToList $ mapBag errMsgSpan $ srcErrorMessages se

   eParsed <- handleSourceError wrapErr (Right <$> parseModule modSummary)

   case eParsed of
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
       let treeEmpty = String ""
       return Trees{treeErrors = errors,
                    treeModule = "",
                    treeParsed = treeEmpty,
                    treeRenamed = treeEmpty,
                    treeTypechecked = treeEmpty,
                    treeExports = treeEmpty}
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
treesForSession :: GhcMonad m => m [Trees]
treesForSession = do
  liftIO $ putStrLn "in treesForSession"
  hscEnv <- getSession
  mapM treesForModSummary $ hsc_mod_graph hscEnv

-- | Generate trees for given sources, when already in GHC
treesForTargetsSrc :: GhcMonad m => FilePath -> [String] -> m [Trees]
treesForTargetsSrc filePath srcs = do
    liftIO $ putStrLn $ "sources:\n" ++ intercalate "\n---\n" srcs
    gbracket
      getSessionDynFlags
      setSessionDynFlags
      $ \dynFlags -> do
        let dynFlags' = treeDumpFlags dynFlags
        void $ setSessionDynFlags dynFlags'

        -- Construct module graph
        setTargets (map (mkTarget filePath) srcs)
        -- void $ load LoadAllTargets

        -- take care of Lexer errors here !
        result <- handleSourceError wrapErr (Right <$> load LoadAllTargets)
        case result of
          Right _ -> treesForSession -- generate each module
          Left failure ->
            let treeEmpty = String "" in
            return [Trees {
                    treeErrors = failure,
                    treeModule = "",
                    treeParsed = treeEmpty,
                    treeRenamed = treeEmpty,
                    treeTypechecked = treeEmpty,
                    treeExports = treeEmpty
                   }]
  where
    mkTarget :: FilePath -> String -> Target
    mkTarget fp s = Target {
        -- TODO ? find better TargetFile
        targetId           = TargetFile fp Nothing -- TargetModule $ mkModuleName "TODO ?"
      , targetAllowObjCode = False
      , targetContents     = Just (stringToStringBuffer s, posixSecondsToUTCTime $ fromIntegral (0 :: Integer))
      }

-- todo: return easy JSON-format
wrapErr :: GhcMonad m => SourceError -> m (Either [String] b)
wrapErr se = return $ Left result where
              sem = srcErrorMessages se
              msgs = errBagToStrList sem
              result = msgs ++ bagToList (mapBag (show . errMsgSpan) sem)

errBagToStrList :: Bag ErrMsg -> [String]
errBagToStrList = map show . reverse . bagToList
