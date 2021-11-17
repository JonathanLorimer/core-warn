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
import Data.Bool (bool)
#endif

------------------------------------------------------------------------------
-- | Pretty print a "large number of coercions" warning.
pprWarnLargeCoerce :: Bool -> [SrcSpan] -> CoreBndr -> CoreStats -> SDoc
pprWarnLargeCoerce terse refs bind stats =
  let srcSpanList = if length refs >= 3
                       then take 3 ((bullet <+>) . ppr <$> refs) <> [text "..."]
                       else (bullet <+>) . ppr <$> refs
      CS{..} = stats
      occ_name = coloured colBlueFg . ppr . getOccName $ bind
      terms = coloured colBlueFg $ ppr cs_tm
      types = coloured colBlueFg $ ppr cs_ty
      coers = coloured colBlueFg $ ppr cs_co
      has_refs =
        case refs of
          [] -> False
          sspan -> any isGoodSrcSpan sspan
      full_msg =
        vcat [ text "Found a large number of coercions in GHC Core."
             , nest 2 $  text " GHC produced a a quadratic number of coercions relative to the number of terms."
                      $$ text "This can happen for expensive type families that are used outside of phantom contexts."
             , blankLine
             , hsep [ text "These coercions were introduced in"
                   , occ_name
                   , text "at these locations:"
                   ]
             , nest 4 (vcat srcSpanList)
             , blankLine
             , sep [ text "Terms:",     terms
                   , text "Types:",     types
                   , text "Coercions:", coers
                   ]
             , text ""
             ]
      terse_msg =
        vcat $
          [ text "big coercion in" <+> occ_name
          , sep
              [ text "terms:", terms
              , text "types:", types
              , text "coercions:", coers
              ]
          ] <>
          [ nest 4 $ vcat srcSpanList
          | has_refs
          ]
   in bool full_msg terse_msg terse


------------------------------------------------------------------------------
-- | Heuristic for whether we should show a "large number of coercions"
-- warning.
shouldWarnLargeCoercion :: CoreStats -> Bool
shouldWarnLargeCoercion CS {cs_tm, cs_co} =
  let quad = cs_tm * floor (logBase @Double 2 $ fromIntegral cs_tm)
   in cs_co >= quad && cs_co > 100


