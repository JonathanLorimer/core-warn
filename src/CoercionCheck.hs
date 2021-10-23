module CoercionCheck (plugin) where

import CoercionCheck.ExtraCoercions
import CoercionCheck.ExtraOccurences
import Control.Monad
import CoreStats
import Data.Foldable
import qualified Data.Map as M
import Data.Maybe (listToMaybe)
import Data.Monoid
import qualified Data.Set as Set
import GhcPlugins hiding (singleton, typeSize, (<>))
import Prelude hiding (lookup)

plugin :: Plugin
plugin =
  defaultPlugin
    { installCoreToDos = install,
      pluginRecompile = const $ pure NoForceRecompile
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
install ss ctds = pure $ coercionCheck (parseOpts ss) : ctds

parseOpts :: [CommandLineOption] -> CoercionCheckOpts
parseOpts = go
  where
    go = foldMap $ \case
      "warn-heavy-coerce" -> CoercionCheckOpts (Endo $ pure True) mempty
      "no-warn-heavy-coerce" -> CoercionCheckOpts (Endo $ pure False) mempty
      "warn-heavy-occs" -> CoercionCheckOpts mempty (Endo $ pure True)
      "no-warn-heavy-occs" -> CoercionCheckOpts mempty (Endo $ pure False)
      _ -> mempty

coercionCheck :: CoercionCheckOpts -> CoreToDo
coercionCheck opts = CoreDoPluginPass "coercionCheck" $ \guts -> do
  let programMap = foldMap tabulateBindExpr $ mg_binds guts
      bindStats = fmap exprStats programMap
      bindNames = tabulateOccs bindStats
      derefMap = derefAllVars bindNames programMap

  when (flip appEndo True $ cco_warnHeavyOccs opts) $
    for_ (M.toList bindNames) \(occ, vars) ->
      when (heavyOcc vars) $
        case (listToMaybe . Set.toList) =<< M.lookup occ derefMap of
          Just name | isGoodSrcSpan (getLoc name) -> warnMsg (heavyOccSDoc name occ vars)
          _ -> pure ()
  when (flip appEndo True $ cco_warnHeavyCoerce opts) $
    for_ (M.toList bindStats) \(coreBndr, coreStats) ->
      when (heavyCoerce coreStats) $ warnMsg (heavyCoerceSDoc coreBndr coreStats)
  pure guts
