{-# LANGUAGE NamedFieldPuns #-}
module CoercionCheck (plugin) where

import Plugins
import Data.Monoid
import CoreMonad (CoreToDo(CoreDoPluginPass))
import HscTypes (ModGuts(ModGuts))
import CoreSyn (CoreProgram)
import Outputable (pprPanic)
import GhcPlugins hiding (singleton, (<>))
import Data.Generics.Aliases
import Data.Generics.Schemes
import CoreStats (coreBindsStats, CoreStats(CS, cs_tm, cs_co), exprStats)
import Data.Map
import Data.Foldable hiding (toList)
import Control.Monad

plugin :: Plugin
plugin = defaultPlugin
          { installCoreToDos = install
          , pluginRecompile = const $ pure NoForceRecompile
          }

install :: CorePlugin
install ss ctds = pure $ coercionCheck : ctds

coercionCheck :: CoreToDo
coercionCheck = CoreDoPluginPass "coercionCheck" $ \guts -> do
  let bindStats = foldMap tabulateBindStats $ mg_binds guts
  for_ (toList bindStats) \(coreBndr, coreStats) ->
    when (heavyCoerce coreStats) $ warnMsg (heavyCoerceSDoc coreBndr coreStats)
  pure guts

heavyCoerceSDoc :: CoreBndr -> CoreStats -> SDoc
heavyCoerceSDoc bind stats = ppr (getOccName bind)
                         <+> ppr (getLoc $ getName bind)
                          $$ ppr stats

heavyCoerce :: CoreStats -> Bool
heavyCoerce CS{cs_tm, cs_co} =
  let quad = cs_tm * floor (log $ fromIntegral cs_tm)
  in cs_co >= quad && cs_co > 100

tabulateBindStats :: Bind CoreBndr -> Map CoreBndr CoreStats
tabulateBindStats
  = \case
      (NonRec var ex) -> singleton var (exprStats ex)
      (Rec ex) -> foldMap (\ (var, expr) -> singleton var (exprStats expr)) ex


