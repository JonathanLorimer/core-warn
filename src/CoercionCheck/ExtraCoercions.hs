{-# LANGUAGE NamedFieldPuns #-}

module CoercionCheck.ExtraCoercions where

import Data.Map (Map)
import qualified Data.Map as M

#if __GLASGOW_HASKELL__ >= 900
import GHC.Core.Stats
import GHC.Plugins
#else
import CoreStats
import GhcPlugins
#endif

heavyCoerceSDoc :: [SrcSpan] -> CoreBndr -> CoreStats -> SDoc
heavyCoerceSDoc refs bind stats =
  ppr (getOccName bind)
    <+> ppr (getSrcSpan bind : refs)
     $$ ppr stats

heavyCoerce :: CoreStats -> Bool
heavyCoerce CS {cs_tm, cs_co} =
  let quad = cs_tm * floor (logBase @Double 2 $ fromIntegral cs_tm)
   in cs_co >= quad && cs_co > 100

tabulateBindExpr :: Bind CoreBndr -> Map CoreBndr CoreExpr
tabulateBindExpr (NonRec var ex) = M.singleton var ex
tabulateBindExpr (Rec ex) = foldMap (uncurry M.singleton) ex
