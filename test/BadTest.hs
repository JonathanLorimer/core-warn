{-# OPTIONS_GHC -ddump-simpl -ddump-to-file #-}
{-# OPTIONS_GHC -fplugin=CoercionCheck #-}

module BadTest where

import Infra
import Data.Proxy

bad :: ()
bad = requireEmptyClass @(ToTree('[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12])) $ Proxy

really_bad :: ()
really_bad = seq (requireEmptyClass @(ToTree('[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12])) Proxy)
                 (requireEmptyClass @(ToTree('[12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1])) Proxy)



