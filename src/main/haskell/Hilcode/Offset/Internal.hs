module Hilcode.Offset.Internal where

import Hilcode.Prelude

newtype Offset = Offset Int
                   deriving (Eq, Ord, Show)

makeOffset ∷ Int → Offset
makeOffset offset
  | offset == 0 = error "Invalid offset; must be not be zero."
  | otherwise = Offset offset

withOffset ∷ Offset → (Int → a) → a
withOffset (Offset offset) f = f offset
