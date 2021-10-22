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
import Data.Set (Set)
import Control.Arrow ((&&&))
import qualified Data.Set as Set
import Data.Data
import Data.Ord

plugin :: Plugin
plugin = defaultPlugin
          { installCoreToDos = install
          , pluginRecompile = const $ pure NoForceRecompile
          }

install :: CorePlugin
install ss ctds = pure $ coercionCheck : ctds

coercionCheck :: CoreToDo
coercionCheck = CoreDoPluginPass "coercionCheck" $ \guts -> do
  let bindStats = foldMap (fmap exprStats . tabulateBindExpr) $ mg_binds guts
      bindNames = tabulateOccs bindStats
  for_ (toList bindNames) \(occName, vars) ->
    when (heavyOcc vars) $ warnMsg (heavyOccSDoc occName vars)
  for_ (toList bindStats) \(coreBndr, coreStats) ->
    when (heavyCoerce coreStats) $ warnMsg (heavyCoerceSDoc coreBndr coreStats)
  pure guts


-- Coercion Case
heavyCoerceSDoc :: CoreBndr -> CoreStats -> SDoc
heavyCoerceSDoc bind stats = ppr (getOccName bind)
                         <+> ppr (getLoc $ getName bind)
                          $$ ppr stats

heavyCoerce :: CoreStats -> Bool
heavyCoerce CS{cs_tm, cs_co} =
  let quad = cs_tm * floor (log $ fromIntegral cs_tm)
  in cs_co >= quad && cs_co > 100

tabulateBindExpr :: Bind CoreBndr -> Map CoreBndr CoreExpr
tabulateBindExpr (NonRec var ex) = singleton var ex
tabulateBindExpr (Rec ex) = foldMap (uncurry singleton) ex

-- Occurence Case
tabulateOccs :: Map CoreBndr CoreStats -> Map OccName (Set Var)
tabulateOccs = fromListWith (<>)
             . fmap (getOccName &&& Set.singleton)
             . keys

maxTypeSize :: Set Var -> Int
maxTypeSize = maximum . Set.map (typeSize . idType)

containsRef :: Data a => Set Name -> a -> Bool
containsRef names =
  getAny . everything mappend
    (mkQ mempty (Any . flip Set.member names))

heavyOcc :: Set Var -> Bool
heavyOcc vars = floor (log . fromIntegral $ maxTypeSize vars) < Set.size vars

heavyOccSDoc :: OccName -> Set Var -> SDoc
heavyOccSDoc name vars = ppr name
                <+> ppr (getLoc . getName . head . Set.toList $ vars )
                 $$ ppr (maxTypeSize vars)
                 $$ ppr (Set.size vars)
                 $$ text "too many occs"
