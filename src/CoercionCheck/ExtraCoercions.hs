{-# LANGUAGE CPP            #-}
{-# LANGUAGE NamedFieldPuns #-}

module CoercionCheck.ExtraCoercions where

import Data.Map (Map)
import qualified Data.Map as M

#if __GLASGOW_HASKELL__ >= 900
import GHC.Core.Stats
import GHC.Plugins
#else
import CoreStats
import GhcPlugins hiding ((<>))
#endif
import PprColour

heavyCoerceSDoc :: [SrcSpan] -> CoreBndr -> CoreStats -> SDoc
heavyCoerceSDoc refs bind stats =
  let srcSpanList = if length refs >= 3
                       then take 3 ((bullet <+>) . ppr <$> refs) <> [text "..."]
                       else (bullet <+>) . ppr <$> refs
      CS{..} = stats
   in text "Found a large number of coercions in GHC Core."
       $$ nest 2 (text "GHC produced a a quadratic number of coercions relative to the number of terms.")
       $$ nest 2 (text "This can happen for expensive type families that are used outside of phantom contexts.")
       $$ text ""
       $$ text "These coercions were introduced in"
          <+> (coloured colBlueFg . ppr . getOccName $ bind)
          <+> text "at these locations:"
       $$ nest 4 (vcat srcSpanList)
       $$ text ""
       $$ text "Terms:" <+> coloured colBlueFg (ppr cs_tm)
          <+> text "Types:" <+> coloured colBlueFg (ppr cs_ty)
          <+> text "Coercions:" <+> coloured colBlueFg (ppr cs_co)
       $$ text ""

heavyCoerce :: CoreStats -> Bool
heavyCoerce CS {cs_tm, cs_co} =
  let quad = cs_tm * floor (logBase @Double 2 $ fromIntegral cs_tm)
   in cs_co >= quad && cs_co > 100

tabulateBindExpr :: Bind CoreBndr -> Map CoreBndr CoreExpr
tabulateBindExpr (NonRec var ex) = M.singleton var ex
tabulateBindExpr (Rec ex) = foldMap (uncurry M.singleton) ex
