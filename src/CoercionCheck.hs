{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

module CoercionCheck (plugin) where

import           Control.Arrow ((&&&))
import           Control.Monad
import           CoreStats (CoreStats(CS, cs_tm, cs_co), exprStats)
import           Data.Data
import           Data.Foldable
import           Data.Generics.Aliases
import           Data.Generics.Schemes
import           Data.IORef
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (listToMaybe)
import           Data.Monoid
import           Data.Ord
import           Data.Set (Set)
import qualified Data.Set as Set
import           GHC (GhcTc)
import           GhcPlugins hiding (singleton, (<>), typeSize)
import           HsBinds
import           HsDumpAst (showAstData, BlankSrcSpan(..))
import           Prelude hiding (lookup)
import           System.IO.Unsafe (unsafePerformIO)
import           TcRnMonad (tcg_binds)
import           TcType (tcSplitNestedSigmaTys)
import           TyCoRep hiding (typeSize)
import HsExpr
import TcEvidence
import Data.Bool (bool)


global_tcg_ref :: IORef (LHsBinds GhcTc)
global_tcg_ref = unsafePerformIO $ newIORef $ error "no tcg_binds set"
{-# NOINLINE global_tcg_ref #-}

plugin :: Plugin
plugin = defaultPlugin
          { installCoreToDos = install
          , pluginRecompile = const $ pure NoForceRecompile
          , typeCheckResultAction = \_ _ tcg -> do
              liftIO $ writeIORef global_tcg_ref $ tcg_binds tcg
              -- pprPanic "binds" $ showAstData NoBlankSrcSpan $ tcg_binds tcg
              pure tcg
          }

data CoercionCheckOpts = CoercionCheckOpts
  { cco_warnHeavyCoerce :: Endo Bool
  , cco_warnHeavyOccs   :: Endo Bool
  }

instance Semigroup CoercionCheckOpts where
  (<>) (CoercionCheckOpts lb4 lb5) (CoercionCheckOpts lb lb3)
    = CoercionCheckOpts
        {cco_warnHeavyCoerce = lb <> lb4, cco_warnHeavyOccs = lb3 <> lb5}

instance Monoid (CoercionCheckOpts) where
  mempty
    = CoercionCheckOpts
        {cco_warnHeavyCoerce = mempty, cco_warnHeavyOccs = mempty}

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
      _                      -> mempty



coercionCheck :: CoercionCheckOpts -> LHsBinds GhcTc -> CoreToDo
coercionCheck opts !binds = CoreDoPluginPass "coercionCheck" $ \guts -> do
  let programMap = foldMap tabulateBindExpr $ mg_binds guts
      bindStats = fmap exprStats programMap
      bindNames = tabulateOccs bindStats

  when (flip appEndo True $ cco_warnHeavyOccs opts) $
    for_ (M.toList bindNames) \(occ, vars) ->
      when (heavyOcc vars) $
        case findRef occ binds of
          locs -> warnMsg (heavyOccSDoc (head locs) occ vars)
  when (flip appEndo True $ cco_warnHeavyCoerce opts) $
    for_ (M.toList bindStats) \(coreBndr, coreStats) ->
      when (heavyCoerce coreStats) $ warnMsg (heavyCoerceSDoc coreBndr coreStats)
  pure guts


-- Coercion Case
heavyCoerceSDoc :: CoreBndr -> CoreStats -> SDoc
heavyCoerceSDoc bind stats = ppr (getOccName bind)
                         <+> ppr (getLoc $ getName bind)
                          $$ ppr stats

heavyCoerce :: CoreStats -> Bool
heavyCoerce CS{cs_tm, cs_co} =
  let quad = cs_tm * floor (logBase @Double 2 $ fromIntegral cs_tm)
  in cs_co >= quad && cs_co > 100

tabulateBindExpr :: Bind CoreBndr -> Map CoreBndr CoreExpr
tabulateBindExpr (NonRec var ex) = M.singleton var ex
tabulateBindExpr (Rec ex) = foldMap (uncurry M.singleton) ex

-- Occurence Case
tabulateOccs :: Map CoreBndr CoreStats -> Map OccName (Set CoreBndr)
tabulateOccs = M.fromListWith (<>)
             . fmap (getOccName &&& Set.singleton)
             . M.keys

biggestType :: Set CoreBndr -> Type
biggestType = rhsType . idType . maximumBy (comparing (typeSize . rhsType . idType))

rhsType :: Type -> Type
rhsType ty =
  case tcSplitNestedSigmaTys ty of
    (_, _, ty') -> ty'

heavyOcc :: Set CoreBndr -> Bool
heavyOcc coreBndrs =
  let amountOfCoreBndrs = Set.size coreBndrs
      biggestTypeSize = typeSize (biggestType coreBndrs)
   in (biggestTypeSize `div` 2) < amountOfCoreBndrs
   && amountOfCoreBndrs > 4

heavyOccSDoc :: SrcSpan -> OccName -> Set CoreBndr -> SDoc
heavyOccSDoc loc name vars = ppr name
                <+> ppr loc
                 $$ text "type: " <+> ppr (biggestType vars)
                 $$ text "type size: " <+> ppr (typeSize $ biggestType vars)
                 $$ text "occ count: " <+> ppr (Set.size vars)


findRef :: Data a => OccName -> a -> [SrcSpan]
findRef occ = everything (<>) $ mkQ mempty $ \case
  L loc (HsWrap _ ev _)
    | isGoodSrcSpan loc ->
        everything (<>)
          (mkQ mempty $ \(v :: Var) -> bool [] [loc] $ getOccName v == occ)
          ev
  (_ :: LHsExpr GhcTc) -> []


typeSize :: Type -> Int
typeSize (LitTy {})               = 1
typeSize (TyVarTy {})             = 1
typeSize (AppTy t1 t2)            = typeSize t1 + typeSize t2
typeSize (FunTy t1 t2)            = typeSize t1 + typeSize t2
typeSize (ForAllTy (Bndr tv _) t) = typeSize (varType tv) + typeSize t
typeSize (TyConApp tc [])          =
  let (kind_vars, _, _) = tcSplitNestedSigmaTys $ tyConKind tc
   in 1 - length kind_vars
typeSize (TyConApp tc ts)          =
  let (kind_vars, _, _) = tcSplitNestedSigmaTys $ tyConKind tc
   in sum (fmap typeSize ts) - length kind_vars
typeSize (CastTy ty _)           = typeSize ty
typeSize (CoercionTy _)          = 0

