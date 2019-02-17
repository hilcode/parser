module Hilcode.IndexSpec (spec) where

import           Control.Exception
import qualified Data.IntMap
import qualified Data.Sequence
import qualified Data.Vector
import           Hilcode.Index.Internal
import           Hilcode.Offset.Internal
import           Hilcode.Prelude
import           Test.Hspec
import           Test.QuickCheck
import           Text.Printf

genValidIndex ∷ Gen Index
genValidIndex
  = do index <- choose (0, 100)
       pure (Index index)

genInvalidIndex ∷ Gen Int
genInvalidIndex = choose (-100, -1)

genValidIndexAndOffset ∷ Gen (Index, Offset)
genValidIndexAndOffset
  = do index@(Index index') <- genValidIndex
       offset <- if index' == 0 then choose (1, 100) else oneof [choose (-index', -1), choose (1, 100)]
       pure (index, Offset offset)

prop_validIndex ∷ Index → Bool
prop_validIndex index@(Index n) = index == makeIndex n

prop_invalidIndex ∷ Int → Expectation
prop_invalidIndex n = evaluate (makeIndex n) `shouldThrow` errorCall "Invalid index; must be non negative."

prop_addOffset ∷ (Index, Offset) → Bool
prop_addOffset (index@(Index n), offset@(Offset m)) = addOffset index offset == makeIndex (n + m)

prop_ShowIndex ∷ Index → Bool
prop_ShowIndex index@(Index n) = show index == show n

prop_FormatIndex ∷ Index → Bool
prop_FormatIndex index@(Index n) = printf "%i" index == show n

spec ∷ Spec
spec
  = do describe "Index::makeIndex" $
         do it "makeIndex (-1)" $ evaluate (makeIndex (-1)) `shouldThrow` errorCall "Invalid index; must be non negative."
            it "makeIndex 0" $ makeIndex 0 `shouldBe` Index 0
            it "prop_validIndex" $ property $ forAll genValidIndex prop_validIndex
            it "prop_invalidIndex" $ property $ forAll genInvalidIndex prop_invalidIndex
       describe "Index::zero" $ it "zero" $ zero `shouldBe` Index 0
       describe "Index::addOffset" $
         do it "addOffset (Index 0) (Offset (-1))" $ evaluate (addOffset (Index 0) (Offset (-1))) `shouldThrow` errorCall "Invalid index; must be non negative."
            it "prop_addOffset" $ property $ forAll genValidIndexAndOffset prop_addOffset
       describe "Index::Show" $
         do it "show (Index 1)" $ show (Index 1) `shouldBe` "1"
            it "prop_ShowIndex" $ property $ forAll genValidIndex prop_ShowIndex
       describe "Index::PrintfArg" $
         do it "formatArg (Index 1)" $ printf "%i" (Index 1) `shouldBe` "1"
            it "prop_FormatIndex" $ property $ forAll genValidIndex prop_FormatIndex
       describe "Index::HasGet" $
         do it "[] ! (Index 1)" $ ['a', 'b', 'c'] ! Index 1 `shouldBe` 'b'
            it "Seq ! (Index 1)" $ Data.Sequence.fromList ['a', 'b', 'c'] ! Index 1 `shouldBe` 'b'
            it "Vector ! (Index 1)" $ Data.Vector.fromList ['a', 'b', 'c'] ! Index 1 `shouldBe` 'b'
            it "IntMap ! (Index 1)" $ Data.IntMap.fromList [(0, 'a'), (1, 'b'), (2, 'c')] ! Index 1 `shouldBe` 'b'
       describe "Index::HasGet" $ it "put IntMap (Index 1) 'b'" $ put (Index 1) 'b' Data.IntMap.empty `shouldBe` Data.IntMap.insert 1 'b' Data.IntMap.empty
