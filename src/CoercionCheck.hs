{-# LANGUAGE NamedFieldPuns #-}

module CoercionCheck (plugin) where

import           Control.Arrow ((&&&))
import           Control.Monad
import           CoreMonad (CoreToDo(CoreDoPluginPass))
import           CoreStats (coreBindsStats, CoreStats(CS, cs_tm, cs_co), exprStats)
import           CoreSyn (CoreProgram)
import           Data.Data
import           Data.Foldable
import           Data.Generics.Aliases
import           Data.Generics.Schemes
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (listToMaybe)
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

install :: CorePlugin
install ss ctds = pure $ coercionCheck : ctds

coercionCheck :: CoreToDo
coercionCheck = CoreDoPluginPass "coercionCheck" $ \guts -> do
  let programMap = foldMap tabulateBindExpr $ mg_binds guts
      bindStats = fmap exprStats programMap
      bindNames = tabulateOccs bindStats
      derefMap = derefAllVars bindNames programMap

  for_ (M.toList bindNames) \(occName, vars) ->
    when (heavyOcc vars) $
      case join $ fmap (listToMaybe . Set.toList) $ M.lookup occName derefMap of
        Just name | isGoodSrcSpan (getLoc name)  -> warnMsg (heavyOccSDoc name occName vars)
        _ -> pure ()
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
  let quad = cs_tm * floor (logBase 2 $ fromIntegral cs_tm)
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
heavyOcc coreBndrs =
  let amountOfCoreBndrs = Set.size coreBndrs
      biggestTypeSize = typeSize (biggestType coreBndrs)
   in ceiling ((fromIntegral biggestTypeSize) / 2) < amountOfCoreBndrs
   && amountOfCoreBndrs > 1

heavyOccSDoc :: Name -> OccName -> Set CoreBndr -> SDoc
heavyOccSDoc ref name vars = ppr name
                <+> ppr (getLoc ref)
                 $$ text "type: " <+> ppr (biggestType vars)
                 $$ text "type size: " <+> ppr (typeSize $ biggestType vars)
                 $$ text "occ count: " <+> ppr (Set.size vars)
                 $$ text "sqrt size: " <+> ppr @Int (ceiling (sqrt . fromIntegral . typeSize $ biggestType vars))

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

