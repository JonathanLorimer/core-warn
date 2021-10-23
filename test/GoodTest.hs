{-# OPTIONS_GHC -ddump-simpl -ddump-to-file #-}
{-# OPTIONS_GHC -fplugin=CoreWarn #-}

module GoodTest where

import Infra
import Data.Proxy

good :: ()
good = requireEmptyClass $ Proxy @(ToTree('[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]))


