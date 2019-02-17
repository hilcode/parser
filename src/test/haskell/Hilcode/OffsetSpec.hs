module Hilcode.OffsetSpec (spec) where

import Control.Exception
import Hilcode.Offset.Internal
import Hilcode.Prelude
import Test.Hspec
import Test.QuickCheck

genValidOffset ∷ Gen Offset
genValidOffset
  = do offset <- oneof [choose (-100, -1), choose (1, 100)]
       pure <| Offset offset

prop_validOffset ∷ Offset → Bool
prop_validOffset offset@(Offset n) = offset == makeOffset n

spec ∷ Spec
spec
  = describe "Offset::makeOffset" $
      do it "makeOffset 0" $ evaluate (makeOffset 0) `shouldThrow` errorCall "Invalid offset; must be not be zero."
         it "makeOffset 1" $ makeOffset 1 `shouldBe` Offset 1
         it "makeOffset (-1)" $ makeOffset (-1) `shouldBe` Offset (-1)
         it "prop_validOffset" $ property $ forAll genValidOffset prop_validOffset
