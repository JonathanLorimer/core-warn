{-# OPTIONS_GHC -ddump-simpl -ddump-to-file #-}

module BadTest where

import Infra
import Data.Proxy

bad :: ()
bad = requireEmptyClass @(ToTree('[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12])) $ Proxy



