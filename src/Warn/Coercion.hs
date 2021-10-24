{-# LANGUAGE CPP            #-}
{-# LANGUAGE NamedFieldPuns #-}

module Warn.Coercion where

#if __GLASGOW_HASKELL__ >= 900
import GHC.Core.Stats
import GHC.Plugins hiding ((<>))
import GHC.Utils.Ppr.Colour
#else
import CoreStats
import GhcPlugins hiding ((<>))
import PprColour
#endif

------------------------------------------------------------------------------
-- | Pretty print a "large number of coercions" warning.
pprWarnLargeCoerce :: [SrcSpan] -> CoreBndr -> CoreStats -> SDoc
pprWarnLargeCoerce refs bind stats =
  let srcSpanList = if length refs >= 3
                       then take 3 ((bullet <+>) . ppr <$> refs) <> [text "..."]
                       else (bullet <+>) . ppr <$> refs
      CS{..} = stats
   in vcat [ text "Found a large number of coercions in GHC Core."
           , nest 2 $  text " GHC produced a a quadratic number of coercions relative to the number of terms."
                    $$ text "This can happen for expensive type families that are used outside of phantom contexts."
           , blankLine
           , hsep [ text "These coercions were introduced in"
                 , coloured colBlueFg . ppr . getOccName $ bind
                 , text "at these locations:"
                 ]
           , nest 4 (vcat srcSpanList)
           , blankLine
           , sep [ text "Terms:",     coloured colBlueFg $ ppr cs_tm
                 , text "Types:",     coloured colBlueFg $ ppr cs_ty
                 , text "Coercions:", coloured colBlueFg $ ppr cs_co
                 ]
           , text ""
           ]


------------------------------------------------------------------------------
-- | Heuristic for whether we should show a "large number of coercions"
-- warning.
shouldWarnLargeCoercion :: CoreStats -> Bool
shouldWarnLargeCoercion CS {cs_tm, cs_co} =
  let quad = cs_tm * floor (logBase @Double 2 $ fromIntegral cs_tm)
   in cs_co >= quad && cs_co > 100


