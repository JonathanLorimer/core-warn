{-# LANGUAGE NamedFieldPuns #-}

module CoercionCheck (plugin) where

import           Control.Arrow ((&&&))
import           Control.Monad
import           CoreMonad (CoreToDo(CoreDoPluginPass))
import           CoreStats (coreBindsStats, CoreStats(CS, cs_tm, cs_co), exprStats)
import           CoreSyn (CoreProgram)
import           Data.Data
import           Data.Foldable
import           Data.Functor.Identity
import           Data.Generics.Aliases
import           Data.Generics.Schemes
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (listToMaybe, fromMaybe)
import           Data.Monoid
import           Data.Ord
import           Data.Set (Set)
import qualified Data.Set as Set
import           GhcPlugins hiding (singleton, (<>), typeSize)
import           HscTypes (ModGuts(ModGuts))
import           Outputable (pprPanic)
import           Plugins
import           Prelude hiding (lookup)
import           TcType (tcSplitSigmaTy, tcSplitNestedSigmaTys)
import           TyCoRep hiding (typeSize)


plugin :: Plugin
plugin = defaultPlugin
          { installCoreToDos = install
          , pluginRecompile = const $ pure NoForceRecompile
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
install ss ctds = pure $ coercionCheck (parseOpts ss) : ctds

parseOpts :: [CommandLineOption] -> CoercionCheckOpts
parseOpts = go
  where
    go = foldMap $ \case
      "warn-heavy-coerce"    -> CoercionCheckOpts (Endo $ pure True) mempty
      "no-warn-heavy-coerce" -> CoercionCheckOpts (Endo $ pure False) mempty
      "warn-heavy-occs"      -> CoercionCheckOpts mempty (Endo $ pure True)
      "no-warn-heavy-occs"   -> CoercionCheckOpts mempty (Endo $ pure False)



coercionCheck :: CoercionCheckOpts -> CoreToDo
coercionCheck opts = CoreDoPluginPass "coercionCheck" $ \guts -> do
  let programMap = foldMap tabulateBindExpr $ mg_binds guts
      bindStats = fmap exprStats programMap
      bindNames = tabulateOccs bindStats
      derefMap = derefAllVars bindNames programMap

  when (flip appEndo True $ cco_warnHeavyOccs opts) $
    for_ (M.toList bindNames) \(occName, vars) ->
      when (heavyOcc vars) $
        case join $ fmap (listToMaybe . Set.toList) $ M.lookup occName derefMap of
          Just name | isGoodSrcSpan (getLoc name)  -> warnMsg (heavyOccSDoc name occName vars)
          _ -> pure ()
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
  let quad = cs_tm * floor (log $ fromIntegral cs_tm)
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

containsRef :: Data a => Set CoreBndr -> a -> Bool
containsRef names =
  getAny . everything mappend
    (mkQ mempty (Any . flip Set.member names))

heavyOcc :: Set CoreBndr -> Bool
heavyOcc vars =
  let occs = Set.size vars
      biggest = biggestType vars
      types = typeSize biggest
   in ceiling (log $ fromIntegral occs) <  types && occs > 1

heavyOccSDoc :: Name -> OccName -> Set CoreBndr -> SDoc
heavyOccSDoc ref name vars = ppr name
                <+> ppr (getLoc ref)
                 $$ text "type: " <+> ppr (biggestType vars)
                 $$ text "type size: " <+> ppr (typeSize $ biggestType vars)
                 $$ text "occ count: " <+> ppr (Set.size vars)

-- whichProgramsContainThisOccNameEquivalenceClass?
derefVar
    :: Set CoreBndr           -- equivalence class of names with the same occname
    -> Map CoreBndr CoreExpr  -- all names to programs
    -> Set Name               -- names of programs that contain any of the equivalence
derefVar vars exprs = Set.fromList $ do
  (bndr, expr) <- M.toList exprs
  guard $ containsRef vars expr
  pure $ getName bndr

-- map from occnames to programs which reference it
derefAllVars
    :: Map OccName (Set CoreBndr)
    -> Map CoreBndr CoreExpr
    -> Map OccName (Set Name)
derefAllVars ms exprs = M.fromList $ do
  (occ, vars) <- M.toList ms
  let x = derefVar vars exprs
  pure (occ, Set.filter ((/= occ) . getOccName) x)

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
typeSize (CastTy ty co)           = typeSize ty + coercionSize co
typeSize (CoercionTy co)          = 0

