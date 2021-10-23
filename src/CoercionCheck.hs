module CoercionCheck (plugin) where

import CoercionCheck.ExtraCoercions
import CoercionCheck.ExtraOccurences
import Control.Monad
import CoreStats
import Data.Foldable
import qualified Data.Map as M
import Data.Monoid
import GhcPlugins hiding (singleton, typeSize, (<>))
import Prelude hiding (lookup)
import HsBinds (LHsBinds)
import GHC (GhcTc)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import TcRnMonad (tcg_binds)
import Generics.SYB
import HsExpr
import Data.Bool (bool)

global_tcg_ref :: IORef (LHsBinds GhcTc)
global_tcg_ref = unsafePerformIO $ newIORef $ error "no tcg_binds set"
{-# NOINLINE global_tcg_ref #-}

plugin :: Plugin
plugin =
  defaultPlugin
    { installCoreToDos = install
    , pluginRecompile = const $ pure NoForceRecompile
    , typeCheckResultAction = \_ _ tcg -> do
        liftIO $ writeIORef global_tcg_ref $ tcg_binds tcg
        pure tcg
    }

data CoercionCheckOpts = CoercionCheckOpts
  { cco_warnHeavyCoerce :: Endo Bool,
    cco_warnHeavyOccs :: Endo Bool
  }

instance Semigroup CoercionCheckOpts where
  (<>) (CoercionCheckOpts lb4 lb5) (CoercionCheckOpts lb lb3) =
    CoercionCheckOpts
      { cco_warnHeavyCoerce = lb <> lb4,
        cco_warnHeavyOccs = lb3 <> lb5
      }

instance Monoid CoercionCheckOpts where
  mempty =
    CoercionCheckOpts
      { cco_warnHeavyCoerce = mempty,
        cco_warnHeavyOccs = mempty
      }

install :: CorePlugin
install ss ctds = do
  binds <- liftIO $ readIORef global_tcg_ref
  pure $ coercionCheck (parseOpts ss) binds : ctds

parseOpts :: [CommandLineOption] -> CoercionCheckOpts
parseOpts = go
  where
    go = foldMap $ \case
      "warn-heavy-coerce" -> CoercionCheckOpts (Endo $ pure True) mempty
      "no-warn-heavy-coerce" -> CoercionCheckOpts (Endo $ pure False) mempty
      "warn-heavy-occs" -> CoercionCheckOpts mempty (Endo $ pure True)
      "no-warn-heavy-occs" -> CoercionCheckOpts mempty (Endo $ pure False)
      _ -> mempty

findRef :: Data a => OccName -> a -> [SrcSpan]
findRef occ = everything (<>) $ mkQ mempty $ \case
  L loc (HsWrap _ ev _)
    | isGoodSrcSpan loc ->
        everything (<>)
          (mkQ mempty $ \(v :: Var) -> bool [] [loc] $ getOccName v == occ)
          ev
  (_ :: LHsExpr GhcTc) -> []

coercionCheck :: CoercionCheckOpts -> LHsBinds GhcTc -> CoreToDo
coercionCheck opts binds = CoreDoPluginPass "coercionCheck" $ \guts -> do
  let programMap = foldMap tabulateBindExpr $ mg_binds guts
      bindStats = fmap exprStats programMap
      bindNames = tabulateOccs bindStats

  when (flip appEndo True $ cco_warnHeavyOccs opts) $
    for_ (M.toList bindNames) \(occ, vars) ->
      when (heavyOcc vars) $
        warnMsg (heavyOccSDoc (findRef occ binds) occ vars)
  when (flip appEndo True $ cco_warnHeavyCoerce opts) $
    for_ (M.toList bindStats) \(coreBndr, coreStats) ->
      when (heavyCoerce coreStats) $ warnMsg (heavyCoerceSDoc coreBndr coreStats)
  pure guts
