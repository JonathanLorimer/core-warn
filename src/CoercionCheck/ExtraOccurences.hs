{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 900
#define FUNTYARG _ _
#elif __GLASGOW_HASKELL__ == 810
#define FUNTYARG _
#else
#define FUNTYARG
#endif

module CoercionCheck.ExtraOccurences where

import Data.Foldable
import Data.Generics.Aliases
import Data.Generics.Schemes
import Data.Map (Map)
import Data.Ord
import Data.Set (Set)
import qualified Data.Set as Set

#if __GLASGOW_HASKELL__ >= 900
import GHC.Plugins hiding ((<>))
import GHC.Tc.Utils.TcType (tcSplitNestedSigmaTys)
import GHC.Core.TyCo.Rep
#else
import GhcPlugins hiding ((<>))
import TcType (tcSplitNestedSigmaTys)
import TyCoRep
#endif

mkCoreAdjacencyMap :: Map CoreBndr CoreExpr -> Map CoreBndr (Set CoreBndr)
mkCoreAdjacencyMap = fmap $ everything mappend (mkQ mempty Set.singleton)

biggestType :: Set CoreBndr -> Type
biggestType = rhsType . idType . maximumBy (comparing (typeSizeWithoutKinds . rhsType . idType))

rhsType :: Type -> Type
rhsType ty =
  case tcSplitNestedSigmaTys ty of
    (_, _, ty') -> ty'

heavyOcc :: Set CoreBndr -> Bool
heavyOcc coreBndrs =
  let amountOfCoreBndrs = Set.size coreBndrs
      biggestTypeSize = typeSizeWithoutKinds (biggestType coreBndrs)
   in biggestTypeSize `div` 2 < amountOfCoreBndrs
        && amountOfCoreBndrs > 4

heavyOccSDoc :: [SrcSpan] -> OccName -> Set CoreBndr -> SDoc
heavyOccSDoc refs name vars =
  ppr name
    <+> ppr refs
      $$ text "type: "
    <+> ppr (biggestType vars)
      $$ text "type size: "
    <+> ppr (typeSizeWithoutKinds $ biggestType vars)
      $$ text "occ count: "
    <+> ppr (Set.size vars)

typeSizeWithoutKinds :: Type -> Int
typeSizeWithoutKinds LitTy {} = 1
typeSizeWithoutKinds TyVarTy {} = 1
typeSizeWithoutKinds (AppTy t1 t2) = typeSizeWithoutKinds t1 + typeSizeWithoutKinds t2
typeSizeWithoutKinds (FunTy FUNTYARG t1 t2) = typeSizeWithoutKinds t1 + typeSizeWithoutKinds t2
typeSizeWithoutKinds (ForAllTy (Bndr tv _) t) = typeSizeWithoutKinds (varType tv) + typeSizeWithoutKinds t
typeSizeWithoutKinds (TyConApp tc []) =
  let (kind_vars, _, _) = tcSplitNestedSigmaTys $ tyConKind tc
   in 1 - length kind_vars
typeSizeWithoutKinds (TyConApp tc ts) =
  let (kind_vars, _, _) = tcSplitNestedSigmaTys $ tyConKind tc
   in sum (fmap typeSizeWithoutKinds ts) - length kind_vars
typeSizeWithoutKinds (CastTy ty _) = typeSizeWithoutKinds ty
typeSizeWithoutKinds (CoercionTy _) = 0

