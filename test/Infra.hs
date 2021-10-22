module Infra (requireEmptyClass, ToTree) where

import Data.Kind (Type)
import Data.Proxy

data Tree a =
    Zero
  | One a
  | Two a a
  | Branch a (Tree a) (Tree a)

class EmptyClass (xs :: Tree a) where

instance EmptyClass 'Zero
instance EmptyClass ('One x)
instance EmptyClass ('Two x1 x2)
instance (EmptyClass l, EmptyClass r) => EmptyClass ('Branch x l r)

-- Evens [0, 1, .. 9] == [0, 2, 4, 6, 8]
type family Evens (xs :: [a]) :: [a] where
  Evens '[]            = '[]
  Evens '[x]           = '[x]
  Evens (x ': _ ': xs) = x ': Evens xs

-- Odds [0, 1, .. 9] == [1, 3, 5, 7, 9]
type family Odds (xs :: [a]) :: [a] where
  Odds '[]       = '[]
  Odds (_ ': xs) = Evens xs

type family ToTree (xs :: [a]) :: Tree a where
  ToTree '[]       = 'Zero
  ToTree '[x]      = 'One x
  ToTree '[x1, x2] = 'Two x1 x2
  ToTree (x ': xs) = 'Branch x (ToTree (Evens xs)) (ToTree (Odds xs))

requireEmptyClass :: EmptyClass xs => Proxy xs -> ()
requireEmptyClass _ = ()

