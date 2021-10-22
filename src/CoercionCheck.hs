module CoercionCheck (plugin) where

import Plugins
import CoreMonad (CoreToDo(CoreDoPluginPass))
import HscTypes (ModGuts(ModGuts))
import CoreSyn (CoreProgram)
import Outputable (pprPanic)
import GhcPlugins

plugin :: Plugin
plugin = defaultPlugin
          { installCoreToDos = install
          , pluginRecompile = const $ pure NoForceRecompile
          }

install :: CorePlugin
install ss ctds = pure $ coercionCheck : ctds

coercionCheck :: CoreToDo
coercionCheck = CoreDoPluginPass "coercionCheck" $ \guts -> pure $
  guts{ mg_binds = updateBinds $  mg_binds guts}

updateBinds :: CoreProgram -> CoreProgram
updateBinds bis = pprPanic "ahhhhh" $ ppr bis

-- $>

