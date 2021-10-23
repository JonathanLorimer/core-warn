{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}

#if __GLASGOW_HASKELL__ >= 810
#define REASON NoReason
#else
#define REASON
#endif

#if __GLASGOW_HASKELL__ >= 900
#define VARBINDARG
#else
#define VARBINDARG _
#endif

module CoercionCheck (plugin) where

import CoercionCheck.ExtraCoercions
import CoercionCheck.ExtraOccurences
import Control.Monad
import Data.Bool (bool)
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable
import Data.Graph.Good
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Monoid
import GHC (GhcTc)
import Generics.SYB
import Prelude hiding (lookup)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map as M
import qualified Data.Set as S

#if __GLASGOW_HASKELL__ >= 900
import GHC.Core.Stats
import GHC.Plugins hiding (typeSize, (<>))
import GHC.Tc.Types  (tcg_binds)
#else
import CoreStats
import GhcPlugins hiding (typeSize, (<>))
import TcRnMonad (tcg_binds)
#endif

#if __GLASGOW_HASKELL__ >= 810
import GHC.Hs.Expr
import GHC.Hs.Binds
#else
import HsExpr
import HsBinds
#endif

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
      "warn-heavy-coerce"    -> CoercionCheckOpts (Endo $ pure True) mempty
      "no-warn-heavy-coerce" -> CoercionCheckOpts (Endo $ pure False) mempty
      "warn-heavy-occs"      -> CoercionCheckOpts mempty (Endo $ pure True)
      "no-warn-heavy-occs"   -> CoercionCheckOpts mempty (Endo $ pure False)
      _ -> mempty

findRef :: Data a => OccName -> a -> [SrcSpan]
findRef occ = everything (<>) $ mkQ mempty $ \case
#if __GLASGOW_HASKELL__ >= 900
  L loc (XExpr (WrapExpr ev))
#else
  L loc (HsWrap _ ev _)
#endif
    | isGoodSrcSpan loc ->
        everything (<>)
          (mkQ mempty $ \(v :: Var) -> bool [] [loc] $ getOccName v == occ)
          ev
  (_ :: LHsExpr GhcTc) -> []

------------------------------------------------------------------------------
-- | Given an 'OccName', find the src span for every coercion inside of its
-- definition.
findBindCoercions :: Data a => Name -> a -> [SrcSpan]
findBindCoercions occ = everything (<>) $ mkQ mempty $ \case
  x@(VarBind _ a _ VARBINDARG)
    | getName a == occ ->
        get_sub x
  x@(FunBind _ (L _ a) _ _ _)
    | getName a == occ ->
        get_sub x
  x@(AbsBinds _ _ b e _ _ _)
    | any ((== occ) . getName) b
   || any ((== occ) . getName . abe_poly) e -> get_sub x
  (_ :: HsBindLR GhcTc GhcTc) -> []
  where
    get_sub x =
      everything (<>) (mkQ mempty $ \case
#if __GLASGOW_HASKELL__ >= 900
        L loc (XExpr (WrapExpr y))
#else
        L loc (HsWrap _ y _)
#endif
          | isGoodSrcSpan loc
          , gtypecount (undefined :: Coercion) y > 0  -> [loc]
        (_ :: LHsExpr GhcTc) -> []
                      ) x

insertIfEmpty :: a -> [a] -> [a]
insertIfEmpty a as =
  case null as of
    True -> [a]
    False -> as


isDictVar :: CoreBndr -> Bool
isDictVar bndr = fromMaybe False $ do
  (tycon, _) <- tcSplitTyConApp_maybe $ idType bndr
  _cls <- tyConClass_maybe tycon
  pure True


coercionCheck :: CoercionCheckOpts -> LHsBinds GhcTc -> CoreToDo
coercionCheck opts binds = CoreDoPluginPass "coercionCheck" $ \guts -> do
  let programMap = foldMap tabulateBindExpr $ mg_binds guts
      dictSets = fmap (S.fromList . toList)
                 . components
                 . graphFromEdges
                 . filter (isDictVar . fst)
                 . M.toList . fmap S.toList
                 . mkCoreAdjacencyMap
                 $ programMap

  when (flip appEndo True $ cco_warnHeavyOccs opts) $
    for_ dictSets \dictSet ->
      -- warnMsg REASON $ ppr dictSet
      when (heavyOcc dictSet) $
        warnMsg REASON . ppr $ foldMap (S.singleton . occName) dictSet
          -- heavyOccSDoc (nubOrd $ findRef occ binds) occ vars
  when (flip appEndo True $ cco_warnHeavyCoerce opts) $
    for_ (M.toList . fmap exprStats $ programMap) \(coreBndr, coreStats) ->
      when (heavyCoerce coreStats) $
        warnMsg REASON $
          heavyCoerceSDoc
            (insertIfEmpty noSrcSpan $ nubOrd $ findBindCoercions (getName coreBndr) binds)
            coreBndr
            coreStats
  pure guts
