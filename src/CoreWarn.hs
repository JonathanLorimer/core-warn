{-# LANGUAGE CPP #-}

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

module CoreWarn (plugin) where

import Warn.Coercion
import Warn.Dictionary
import Control.Monad
import Data.Bool (bool)
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
import Data.List (nub)
#else
import Data.Containers.ListUtils (nubOrd)
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

------------------------------------------------------------------------------
-- | The main @core-warn@ program.
plugin :: Plugin
plugin =
  defaultPlugin
    { installCoreToDos = \ss ctds -> do
        binds <- liftIO $ readIORef global_tcg_ref
        pure $ ctds ++ [coreWarn (parseOpts ss) binds]
    , pluginRecompile = const $ pure NoForceRecompile
    , typeCheckResultAction = \_ _ tcg -> do
        liftIO $ writeIORef global_tcg_ref $ tcg_binds tcg
        pure tcg
    }


------------------------------------------------------------------------------
-- | We need to get our grubby little hands on the 'tcg_binds', but the
-- core-to-core plugin interface doesn't give us access to them. So we do this
-- very safe trick to get a hold of them.
global_tcg_ref :: IORef (LHsBinds GhcTc)
global_tcg_ref = unsafePerformIO $ newIORef $ error "no tcg_binds set"
{-# NOINLINE global_tcg_ref #-}


------------------------------------------------------------------------------
-- | Options for @core-warn@. These are opt-out.
data CoreWarnOpts = CoreWarnOpts
  { cwo_warnBigCoerces :: Endo Bool,
    cwo_warnDeepDicts :: Endo Bool
  , cwo_terseMessages :: Endo Bool
  }

instance Semigroup CoreWarnOpts where
  (CoreWarnOpts en en' en2) <> (CoreWarnOpts en3 en4 en5)
    = CoreWarnOpts
        {cwo_warnBigCoerces = en <> en3, cwo_warnDeepDicts = en' <> en4,
         cwo_terseMessages = en2 <> en5}

instance Monoid CoreWarnOpts where
  mempty
    = CoreWarnOpts
        {cwo_warnBigCoerces = mempty, cwo_warnDeepDicts = mempty,
         cwo_terseMessages = mempty}


------------------------------------------------------------------------------
-- | Parse options.
parseOpts :: [CommandLineOption] -> CoreWarnOpts
parseOpts = go
  where
    go = foldMap $ \case
      "warn-large-coercions"    -> CoreWarnOpts (Endo $ pure True) mempty mempty
      "no-warn-large-coercions" -> CoreWarnOpts (Endo $ pure False) mempty mempty
      "warn-deep-dicts"         -> CoreWarnOpts mempty (Endo $ pure True) mempty
      "no-warn-deep-dicts"      -> CoreWarnOpts mempty (Endo $ pure False) mempty
      "terse-messages"          -> CoreWarnOpts mempty mempty (Endo $ pure True)
      "no-terse-messages"       -> CoreWarnOpts mempty mempty (Endo $ pure False)
      _ -> mempty


------------------------------------------------------------------------------
-- | Given an 'OccName' corresponding to a dictionary, find every immediate
-- 'SrcSpan's that contain it.
findDictRef :: Data a => OccName -> a -> [SrcSpan]
findDictRef occ = everything (<>) $ mkQ mempty $ \case
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
  x@(FunBind _ (L _ a) _ _ VARBINDARG)
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


------------------------------------------------------------------------------
-- | Like 'fromMaybe' but for lists.
singletonIfEmpty :: a -> [a] -> [a]
singletonIfEmpty a as = if null as then [a] else as


------------------------------------------------------------------------------
-- | Is this 'CoreBndr' the 'Var' of a dictionary?
isDictVar :: CoreBndr -> Bool
isDictVar bndr = fromMaybe False $ do
  (tycon, _) <- tcSplitTyConApp_maybe $ idType bndr
  _cls <- tyConClass_maybe tycon
  pure True


------------------------------------------------------------------------------
-- | Translatea @'Bind' 'CoreBndr'@ into a map from 'CoreBndr's to 'CoreExpr's.
coreBndrToExprMap :: Bind CoreBndr -> M.Map CoreBndr CoreExpr
coreBndrToExprMap (NonRec var ex) = M.singleton var ex
coreBndrToExprMap (Rec ex) = foldMap (uncurry M.singleton) ex


------------------------------------------------------------------------------
-- | The @core-warn@ todo pass.
coreWarn :: CoreWarnOpts -> LHsBinds GhcTc -> CoreToDo
coreWarn opts binds = CoreDoPluginPass "coercionCheck" $ \guts -> do
  let programMap = foldMap coreBndrToExprMap $ mg_binds guts
      dictSets = fmap (S.fromList . toList)
                 . components
                 . graphFromEdges
                 . filter (isDictVar . fst)
                 . M.toList . fmap S.toList
                 . mkCoreAdjacencyMap
                 $ programMap

  when (flip appEndo True $ cwo_warnDeepDicts opts) $
    for_ dictSets \dictSet -> do
      let srcSpans
            = filter isGoodSrcSpan
            $ foldMap (flip findDictRef binds)
            $ foldMap (S.singleton . occName) dictSet
      when (shouldWarnDeepDict dictSet) $
        warnMsg REASON $
          pprDeepDict
            (appEndo (cwo_terseMessages opts) True)
            srcSpans
            dictSet


  when (flip appEndo True $ cwo_warnBigCoerces opts) $
    for_ (M.toList . fmap exprStats $ programMap) \(coreBndr, coreStats) ->
      when (shouldWarnLargeCoercion coreStats) $
        warnMsg REASON $
          pprWarnLargeCoerce
            (appEndo (cwo_terseMessages opts) True)
            (singletonIfEmpty noSrcSpan $
#if __GLASGOW_HASKELL__ >= 900
              -- TODO(sandy): I know it's slow, but blame GHC9 for getting rid
              -- of the 'Ord' instance on 'SrcSpan
              nub
#else
              nubOrd
#endif
              $ findBindCoercions (getName coreBndr) binds)
            coreBndr
            coreStats
  pure guts

