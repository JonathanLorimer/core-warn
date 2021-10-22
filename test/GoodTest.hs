{-# OPTIONS_GHC -ddump-simpl -ddump-to-file #-}

module GoodTest where

import Infra
import Data.Proxy

good :: ()
blah = requireEmptyClass $ Proxy @(ToTree('[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]))


