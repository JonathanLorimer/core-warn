{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 900
#define FUNTYARG _ _
#elif __GLASGOW_HASKELL__ == 810
#define FUNTYARG _
#else
#define FUNTYARG
#endif

module Warn.Dictionary where

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
import GHC.Utils.Ppr.Colour
#else
import GhcPlugins hiding ((<>))
import TcType (tcSplitNestedSigmaTys)
import TyCoRep
import PprColour
#endif


------------------------------------------------------------------------------
-- | Build an adjacency map from core bindings to the core bindings they
-- reference.
mkCoreAdjacencyMap :: Map CoreBndr CoreExpr -> Map CoreBndr (Set CoreBndr)
mkCoreAdjacencyMap = fmap $ everything mappend (mkQ mempty Set.singleton)


------------------------------------------------------------------------------
-- | Get the biggest type (via 'typeSizeWithoutKinds') in a set.
biggestType :: Set CoreBndr -> Type
biggestType = rhsType . idType . maximumBy (comparing (typeSizeWithoutKinds . rhsType . idType))


------------------------------------------------------------------------------
-- | Remove the forall quantifiers and contexts from a type.
rhsType :: Type -> Type
rhsType ty =
  case tcSplitNestedSigmaTys ty of
    (_, _, ty') -> ty'


------------------------------------------------------------------------------
-- | Heuristic for whether we should show the "deep dicts" warning.
shouldWarnDeepDict :: Set CoreBndr -> Bool
shouldWarnDeepDict coreBndrs =
  let amountOfCoreBndrs = Set.size coreBndrs
      biggestTypeSize = typeSizeWithoutKinds (biggestType coreBndrs)
   in biggestTypeSize `div` 2 < amountOfCoreBndrs
        && amountOfCoreBndrs > 4


------------------------------------------------------------------------------
-- | Pretty print a "deep dicts" warning.
pprDeepDict :: [SrcSpan] -> Set CoreBndr -> SDoc
pprDeepDict goodSpans vars =
  let srcSpanList = if length goodSpans >= 3
                       then take 3 ((bullet <+>) . ppr <$> goodSpans) <> [text "..."]
                       else (bullet <+>) . ppr <$> goodSpans
   in vcat [ text "Found a large chain of dictionaries produced in GHC Core."
           , nest 2 $  text "A big instance chain that is generating a linear amount of core dictionaries."
                    $$ text "This is probably caused by instance induction on an unbalanced structure (like a type-level list)."
                    $$ text "Consider using a balanced structure (like a type-level tree)."
           , blankLine
           , text "Arising from:"
           , nest 4 (vcat srcSpanList)
           , blankLine
           , text "Biggest dictionary: " <+> coloured colBlueFg (ppr $ biggestType vars)
           , text "Size of type: " <+> coloured colBlueFg (int $ typeSizeWithoutKinds $ biggestType vars)
           , text "Number of dictionaries: " <+> coloured colBlueFg (int $ Set.size vars)
           , blankLine
           ]


------------------------------------------------------------------------------
-- | Attempts to measure "how big" a type is. We count terminal type
-- constructors, and type literals as 1. Kinds are right out. Chosen so that
-- @'[1, 2, 3, 4]@ has size 4.
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

