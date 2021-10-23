{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-uniques #-}
{-# OPTIONS_GHC -fplugin=CoercionCheck #-}

module InductionTest where

import Infra
import Data.Proxy

tree :: ()
tree = requireEmptyClass $ Proxy @(ToTree('[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23,  24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40]))

induction :: ()
induction = requireInductionClass @('[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]) $ Proxy



