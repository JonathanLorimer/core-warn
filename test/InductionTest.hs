{-# OPTIONS_GHC -ddump-simpl -ddump-to-file #-}
{-# OPTIONS_GHC -fplugin=CoercionCheck #-}

module InductionTest where

import Infra
import Data.Proxy

induction :: ()
induction = requireInductionClass @('[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12]) $ Proxy




