module CoercionCheck.ExtraOccurences where

import Control.Arrow ((&&&))
import CoreStats
import Data.Data
import Data.Foldable
import Data.Generics.Aliases
import Data.Generics.Schemes
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
import Data.Ord
import Data.Set (Set)
import qualified Data.Set as Set
import GhcPlugins hiding ((<>))
import TcType (tcSplitNestedSigmaTys)
import TyCoRep hiding (typeSize)

tabulateOccs :: Map CoreBndr CoreStats -> Map OccName (Set CoreBndr)
tabulateOccs =
  M.fromListWith (<>)
    . fmap (getOccName &&& Set.singleton)
    . M.keys

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
typeSizeWithoutKinds (AppTy t1 t2) = typeSizeWithoutKinds t1 + typeSize t2
typeSizeWithoutKinds (FunTy t1 t2) = typeSizeWithoutKinds t1 + typeSize t2
typeSizeWithoutKinds (ForAllTy (Bndr tv _) t) = typeSizeWithoutKinds (varType tv) + typeSize t
typeSizeWithoutKinds (TyConApp tc []) =
  let (kind_vars, _, _) = tcSplitNestedSigmaTys $ tyConKind tc
   in 1 - length kind_vars
typeSizeWithoutKinds (TyConApp tc ts) =
  let (kind_vars, _, _) = tcSplitNestedSigmaTys $ tyConKind tc
   in sum (fmap typeSizeWithoutKinds ts) - length kind_vars
typeSizeWithoutKinds (CastTy ty _) = typeSizeWithoutKinds ty
typeSizeWithoutKinds (CoercionTy _) = 0

