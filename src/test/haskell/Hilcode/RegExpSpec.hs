module Hilcode.RegExpSpec (spec) where

import Control.Exception

import qualified Data.Sequence

import Hilcode.Index.Internal
import Hilcode.Offset.Internal
import Hilcode.Prelude
import Hilcode.RegExp.Internal

import Test.Hspec
import Test.QuickCheck

genValidRange ∷ Gen (Char, Char)
genValidRange
  = do ch1 <- arbitrary
       ch2 <- arbitrary
       let lowerBound = min ch1 ch2
           upperBound = max ch1 ch2
         in pure (lowerBound, upperBound)

genMaybeChars ∷ Char → Gen ([Maybe Char], Char)
genMaybeChars ch = (,) <$> elements [[Just ch], [Nothing, Just ch], [Just ch, Nothing], [Nothing, Just ch, Nothing]] <*> pure ch

genMaybes ∷ Gen (Vector (Maybe Char), String)
genMaybes
  = do s <- arbitrary
       maybes <- sequence (genMaybeChars <$> s)
       pure (fromList (concat (fst <$> maybes)), snd <$> maybes)

prop_atom ∷ Char → Bool
prop_atom ch = atom ch == RegExp (Data.Sequence.fromList [StepAtom ch])

prop_validRange ∷ (Char, Char) → Property
prop_validRange (lowerBound, upperBound) = (lowerBound /= upperBound) ==> range lowerBound upperBound == RegExp (Data.Sequence.fromList [StepRange lowerBound upperBound])

prop_invalidRange ∷ (Char, Char) → Property
prop_invalidRange (lowerBound, upperBound) = (lowerBound /= upperBound) ==> evaluate (range upperBound lowerBound) `shouldThrow` errorCall "Invalid bounds: upper bound must be greater than or equal to lower bound."

prop_simpleRange ∷ Char → Bool
prop_simpleRange ch = range ch ch == RegExp (Data.Sequence.fromList [StepAtom ch])

prop_atoms ∷ String → Bool
prop_atoms s = atoms s == RegExp (StepAtom <$> Data.Sequence.fromList s)

prop_repeat ∷ String → Bool
prop_repeat s = repeat (RegExp (Data.Sequence.fromList steps)) == RegExp (Data.Sequence.fromList ([StepFork <| Offset (size steps + 2)] <> steps <> [StepJump <| Offset (-(size steps + 1))]))
  where
      steps ∷ [Step Char Char]
      steps = StepAtom <$> s

prop_or ∷ String → String → Bool
prop_or lft rgt = atoms lft <|> atoms rgt == RegExp (Data.Sequence.fromList ([StepFork (Offset (size lft + 2))] <> steps lft <> [StepJump (Offset (size rgt + 1))] <> steps rgt))
  where
      steps ∷ String → [Step Char Char]
      steps s = StepAtom <$> s

prop_isteps ∷ String → Bool
prop_isteps s = isteps (fromList steps) == fromList (zip (Index <$> [0 ..]) steps)
  where
      steps ∷ [Step Char Char]
      steps = StepAtom <$> s

prop_reduce ∷ (Vector (Maybe Char), String) → Bool
prop_reduce combo = reduce ord (fst combo) == fromList (ord <$> snd combo)

regExpEmpty ∷ RegExp Char Char
regExpEmpty = RegExp empty

regExpA ∷ RegExp Char Char
regExpA = RegExp (Data.Sequence.fromList [StepAtom 'a'])

regExpAB ∷ RegExp Char Char
regExpAB = RegExp (Data.Sequence.fromList [StepAtom 'a', StepAtom 'b'])

regExpABC ∷ RegExp Char Char
regExpABC = RegExp (Data.Sequence.fromList [StepAtom 'a', StepAtom 'b', StepAtom 'c'])

regExpB ∷ RegExp Char Char
regExpB = RegExp (Data.Sequence.fromList [StepAtom 'b'])

spec ∷ Spec
spec
  = do checkRegExpAtom
       checkRegExpRange
       checkRegExpAtoms
       checkRegExpRepeat
       checkRegExpOr
       checkRegExpIsteps
       checkRegExpReduce
       checkRegExpMatchSimple
       checkRegExpMatchComplex

checkRegExpAtom ∷ SpecWith ()
checkRegExpAtom
  = describe "RegExp::atom" $
      do it "atom 'a'" $ atom 'a' `shouldBe` regExpA
         it "prop_atom" $ property prop_atom

checkRegExpRange ∷ SpecWith ()
checkRegExpRange
  = describe "RegExp::range" $
      do it "range 'b' 'a'" $ evaluate (range 'b' 'a') `shouldThrow` errorCall "Invalid bounds: upper bound must be greater than or equal to lower bound."
         it "range 'a' 'z'" $ range 'a' 'z' `shouldBe` RegExp (Data.Sequence.fromList [StepRange 'a' 'z'])
         it "prop_validRange" $ property $ forAll genValidRange prop_validRange
         it "prop_invalidRange" $ property $ forAll genValidRange prop_invalidRange
         it "range 'a' 'a'" $ range 'a' 'a' `shouldBe` regExpA
         it "prop_simpleRange" $ property prop_simpleRange

checkRegExpAtoms ∷ SpecWith ()
checkRegExpAtoms
  = describe "RegExp::atoms" $
      do it "atoms []" $ (atoms [] :: RegExp Char Char) `shouldBe` regExpEmpty
         it "atoms ['a']" $ atoms "a" `shouldBe` regExpA
         it "atoms ['a', 'b']" $ atoms "ab" `shouldBe` regExpAB
         it "atoms ['a', 'b', 'c']" $ atoms "abc" `shouldBe` regExpABC
         it "prop_atoms" $ property prop_atoms

checkRegExpRepeat ∷ SpecWith ()
checkRegExpRepeat
  = describe "RegExp::repeat" $
      do it "repeat (RegExp empty)" $ repeat (RegExp empty :: RegExp Char Char) `shouldBe` RegExp (Data.Sequence.fromList [StepFork (Offset 2), StepJump (Offset (-1))])
         it "repeat (RegExp [StepToken 'a'])" $ repeat regExpA `shouldBe` RegExp (Data.Sequence.fromList [StepFork (Offset 3), StepAtom 'a', StepJump (Offset (-2))])
         it "repeat (RegExp [StepToken 'a', StepToken 'b'])" $ repeat regExpAB `shouldBe` RegExp (Data.Sequence.fromList [StepFork (Offset 4), StepAtom 'a', StepAtom 'b', StepJump (Offset (-3))])
         it "repeat (RegExp [StepToken 'a', StepToken 'b', StepToken 'c'])" $ repeat regExpABC `shouldBe` RegExp (Data.Sequence.fromList [StepFork (Offset 5), StepAtom 'a', StepAtom 'b', StepAtom 'c', StepJump (Offset (-4))])
         it "prop_repeat" $ property prop_repeat

checkRegExpOr ∷ SpecWith ()
checkRegExpOr
  = describe "RegExp::<|>" $
      do it "(RegExp empty) <|> (RegExp empty)" $ regExpEmpty <|> regExpEmpty `shouldBe` RegExp (Data.Sequence.fromList [StepFork (Offset 2), StepJump (Offset 1)])
         it "(RegExp empty) <|> (RegExp [StepToken 'a'])" $ regExpEmpty <|> regExpA `shouldBe` RegExp (Data.Sequence.fromList [StepFork (Offset 2), StepJump (Offset 2), StepAtom 'a'])
         it "(RegExp [StepToken 'a']) <|> (RegExp empty)" $ regExpA <|> regExpEmpty `shouldBe` RegExp (Data.Sequence.fromList [StepFork (Offset 3), StepAtom 'a', StepJump (Offset 1)])
         it "(RegExp [StepToken 'a']) <|> (RegExp [StepToken 'b'])" $ regExpA <|> regExpB `shouldBe` RegExp (Data.Sequence.fromList [StepFork (Offset 3), StepAtom 'a', StepJump (Offset 2), StepAtom 'b'])
         it "prop_or" $ property prop_or

checkRegExpIsteps ∷ SpecWith ()
checkRegExpIsteps
  = describe "RegExp::isteps" $
      do it "isteps []" $ isteps (fromList ([] :: [Step Char Char])) `shouldBe` empty
         it "isteps [StepToken 'a']" $ isteps (fromList [StepAtom 'a']) `shouldBe` fromList [(Index 0, StepAtom 'a')]
         it "isteps [StepToken 'a', StepToken 'b']" $ isteps (fromList [StepAtom 'a', StepAtom 'b']) `shouldBe` fromList [(Index 0, StepAtom 'a'), (Index 1, StepAtom 'b')]
         it "isteps [StepToken 'a', StepToken 'b', StepToken 'c']" $ isteps (fromList [StepAtom 'a', StepAtom 'b', StepAtom 'c']) `shouldBe` fromList [(Index 0, StepAtom 'a'), (Index 1, StepAtom 'b'), (Index 2, StepAtom 'c')]
         it "prop_isteps" $ property prop_isteps

checkRegExpReduce ∷ SpecWith ()
checkRegExpReduce
  = describe "RegExp::reduce" $
      do it "reduce ord []" $ reduce ord empty `shouldBe` empty
         it "reduce ord [Nothing]" $ reduce ord (singleton Nothing) `shouldBe` empty
         it "reduce ord [Just 'A']" $ reduce ord (singleton (Just 'A')) `shouldBe` singleton 65
         it "reduce ord [Nothing, Just 'A']" $ reduce ord (fromList [Nothing, Just 'A']) `shouldBe` singleton 65
         it "reduce ord [Nothing, Just 'A', Nothing, Just 'B']" $ reduce ord (fromList [Nothing, Just 'A', Nothing, Just 'B']) `shouldBe` fromList [65, 66]
         it "prop_reduce" $ property $ forAll genMaybes prop_reduce

checkRegExpMatchSimple ∷ SpecWith ()
checkRegExpMatchSimple
  = describe "RegExp::match (simple)" $
      do it "match (compile (atom 'a')) \"a\"" $ match lexer1 "a" `shouldBe` MATCH "a"
         it "match (compile (atom 'a')) \"\"" $ match lexer1 "" `shouldBe` NO_MATCH
         it "match (compile (atom 'a')) \"b\"" $ match lexer1 "b" `shouldBe` NO_MATCH
         it "match (compile (atom 'a')) \"aa\"" $ match lexer1 "aa" `shouldBe` NO_MATCH
         it "match (compile (range '0' '9')) \"0\"" $ match lexer2 "0" `shouldBe` MATCH "0"
         it "match (compile (range '0' '9')) \"1\"" $ match lexer2 "1" `shouldBe` MATCH "1"
         it "match (compile (range '0' '9')) \"8\"" $ match lexer2 "8" `shouldBe` MATCH "8"
         it "match (compile (range '0' '9')) \"9\"" $ match lexer2 "9" `shouldBe` MATCH "9"
         it "match (compile (range 'b' 'y')) \"a\"" $ match lexer2 "a" `shouldBe` NO_MATCH
         it "match (compile (range 'b' 'y')) \"z\"" $ match lexer2 "z" `shouldBe` NO_MATCH
         it "match (compile ([range 'b' 'y', range '1' '8'])) \"b8\"" $ match lexer3 "b8" `shouldBe` MATCH "b8"
         it "match (compile ([range 'b' 'y', range '1' '8'])) \"a1\"" $ match lexer3 "a1" `shouldBe` NO_MATCH
         it "match (compile ([range 'b' 'y', range '1' '8'])) \"b0\"" $ match lexer3 "b0" `shouldBe` NO_MATCH
         it "match (compile (atoms 'ab')) \"ab\"" $ match lexer4 "ab" `shouldBe` MATCH "ab"
         it "match (compile (atoms 'ab')) \"\"" $ match lexer4 "" `shouldBe` NO_MATCH
         it "match (compile (atoms 'ab')) \"a\"" $ match lexer4 "a" `shouldBe` NO_MATCH
         it "match (compile (atoms 'ab')) \"abc\"" $ match lexer4 "abc" `shouldBe` NO_MATCH
         it "match (compile (atoms 'ab' <|> atoms \"cd\")) \"ab\"" $ match lexer5 "ab" `shouldBe` MATCH "ab"
         it "match (compile (atoms 'ab' <|> atoms \"cd\")) \"cd\"" $ match lexer5 "cd" `shouldBe` MATCH "cd"
         it "match (compile (atoms 'ab' <|> atoms \"cd\")) \"\"" $ match lexer5 "" `shouldBe` NO_MATCH
         it "match (compile (atoms 'ab' <|> atoms \"cd\")) \"a\"" $ match lexer5 "a" `shouldBe` NO_MATCH
         it "match (compile (atoms 'ab' <|> atoms \"cd\")) \"c\"" $ match lexer5 "c" `shouldBe` NO_MATCH
         it "match (compile (mempty <|> atoms \"cd\")) \"cd\"" $ match lexer6 "cd" `shouldBe` MATCH "cd"
         it "match (compile (mempty <|> atoms \"cd\")) \"\"" $ match lexer6 "" `shouldBe` MATCH ""
         it "match (compile (mempty <|> atoms \"cd\")) \"c\"" $ match lexer6 "c" `shouldBe` NO_MATCH
         it "match (compile (atoms 'ab' <|> mempty)) \"ab\"" $ match lexer6' "ab" `shouldBe` MATCH "ab"
         it "match (compile (atoms 'ab' <|> mempty)) \"\"" $ match lexer6' "" `shouldBe` MATCH ""
         it "match (compile (atoms 'ab' <|> mempty)) \"a\"" $ match lexer6' "a" `shouldBe` NO_MATCH
         it "match (compile (mempty <|> mempty)) \"\"" $ match lexer7 "" `shouldBe` MATCH ""
         it "match (compile (mempty <|> mempty)) \"a\"" $ match lexer7 "a" `shouldBe` NO_MATCH
         it "match (compile (repeat mempty)) \"\"" $ match lexer7 "" `shouldBe` MATCH ""
         it "match (compile (repeat mempty)) \"a\"" $ match lexer7 "a" `shouldBe` NO_MATCH
         it "match (compile (repeat (atom 'a'))) \"\"" $ match lexer8 "" `shouldBe` MATCH ""
         it "match (compile (repeat (atom 'a'))) \"a\"" $ match lexer8 "a" `shouldBe` MATCH "a"
         it "match (compile (repeat (atom 'a'))) \"aa\"" $ match lexer8 "aa" `shouldBe` MATCH "aa"
         it "match (compile (repeat (atom 'a'))) \"aab\"" $ match lexer8 "aab" `shouldBe` NO_MATCH
         it "match (compile (repeat (atoms 'ab'))) \"ab\"" $ match lexer9 "ab" `shouldBe` MATCH "ab"
         it "match (compile (repeat (atoms 'ab'))) \"abab\"" $ match lexer9 "abab" `shouldBe` MATCH "abab"
         it "match (compile (repeat (atoms 'ab'))) \"ababa\"" $ match lexer9 "ababa" `shouldBe` NO_MATCH
         it "match (compile (repeat (atom 'a' <|> atom 'b'))) \"\"" $ match lexer10 "" `shouldBe` MATCH ""
         it "match (compile (repeat (atom 'a' <|> atom 'b'))) \"a\"" $ match lexer10 "a" `shouldBe` MATCH "a"
         it "match (compile (repeat (atom 'a' <|> atom 'b'))) \"b\"" $ match lexer10 "b" `shouldBe` MATCH "b"
         it "match (compile (repeat (atom 'a' <|> atom 'b'))) \"ab\"" $ match lexer10 "ab" `shouldBe` MATCH "ab"
         it "match (compile (repeat (atom 'a' <|> atom 'b'))) \"aab\"" $ match lexer10 "aab" `shouldBe` MATCH "aab"
         it "match (compile (repeat (atom 'a' <|> atom 'b'))) \"ba\"" $ match lexer10 "ba" `shouldBe` MATCH "ba"
         it "match (compile (repeat (atom 'a' <|> atom 'b'))) \"bba\"" $ match lexer10 "bba" `shouldBe` MATCH "bba"
         it "match (compile (repeat (atom 'a' <|> atom 'b'))) \"bbac\"" $ match lexer10 "bbac" `shouldBe` NO_MATCH
         it "match (compile (repeat (repeat (atom 'a')))) \"\"" $ match lexer11 "" `shouldBe` MATCH ""
         it "match (compile (repeat (repeat (atom 'a')))) \"a\"" $ match lexer11 "a" `shouldBe` MATCH "a"
         it "match (compile (repeat (repeat (atom 'a')))) \"aaaa\"" $ match lexer12 "aaaa" `shouldBe` MATCH "aaaa"
  where
      lexer1 = makeLexer [TokenDefinition (atom 'a') id]
      lexer2 = makeLexer [TokenDefinition (range '0' '9') id]
      lexer3 = makeLexer [TokenDefinition (range 'b' 'y' <> range '1' '8') id]
      lexer4 = makeLexer [TokenDefinition (atoms "ab") id]
      lexer5 = makeLexer [TokenDefinition (atoms "ab" <|> atoms "cd") id]
      lexer6 = makeLexer [TokenDefinition (mempty <|> atoms "cd") id]
      lexer6' = makeLexer [TokenDefinition (atoms "ab" <|> mempty) id]
      lexer7 = makeLexer [TokenDefinition (mempty <|> mempty) id]
      lexer8 = makeLexer [TokenDefinition (repeat (atom 'a')) id]
      lexer9 = makeLexer [TokenDefinition (repeat (atoms "ab")) id]
      lexer10 = makeLexer [TokenDefinition (repeat (atom 'a' <|> atom 'b')) id]
      lexer11 = makeLexer [TokenDefinition (repeat (repeat (atom 'a'))) id]
      lexer12 = makeLexer [TokenDefinition (repeat (repeat (atom 'a'))) id]

checkRegExpMatchComplex ∷ SpecWith ()
checkRegExpMatchComplex
  = describe "RegExp::match (complex)" $
      do it "match (compile (repeat (mempty <|> atom 'a' <|> atoms \"aa\" <|> repeat (atoms \"aaa\"))))) \"\"" $ match lexer1 "" `shouldBe` MATCH ""
         it "match (compile (repeat (mempty <|> atom 'a' <|> atoms \"aa\" <|> repeat (atoms \"aaa\"))))) \"a\"" $ match lexer1 "a" `shouldBe` MATCH "a"
         it "match (compile (repeat (mempty <|> atom 'a' <|> atoms \"aa\" <|> repeat (atoms \"aaa\"))))) \"aa\"" $ match lexer1 "aa" `shouldBe` MATCH "aa"
         it "match (compile (repeat (mempty <|> atom 'a' <|> atoms \"aa\" <|> repeat (atoms \"aaa\"))))) \"aaa\"" $ match lexer1 "aaa" `shouldBe` MATCH "aaa"
         it "match (compile (repeat (mempty <|> atom 'a' <|> atoms \"aa\" <|> repeat (atoms \"aaa\"))))) \"aaaa\"" $ match lexer1 "aaaa" `shouldBe` MATCH "aaaa"
         it "match (compile (repeat (mempty <|> atom 'a' <|> atoms \"aa\" <|> repeat (atoms \"aaa\"))))) \"aaaaa\"" $ match lexer1 "aaaaa" `shouldBe` MATCH "aaaaa"
         it "match (compile (repeat (mempty <|> atom 'a' <|> atoms \"aa\" <|> repeat (atoms \"aaa\"))))) \"aaaaaa\"" $ match lexer1 "aaaaaa" `shouldBe` MATCH "aaaaaa"
         it "match (compile (repeat (mempty <|> range '1' '8' <|> range '1' '8' <> range '1' '8' <|> repeat (range '1' '8' <> range '1' '8' <> range '1' '8'))))) \"\"" $ match lexer2 "" `shouldBe` MATCH ""
         it "match (compile (repeat (mempty <|> range '1' '8' <|> range '1' '8' <> range '1' '8' <|> repeat (range '1' '8' <> range '1' '8' <> range '1' '8'))))) \"1\"" $ match lexer2 "1" `shouldBe` MATCH "1"
         it "match (compile (repeat (mempty <|> range '1' '8' <|> range '1' '8' <> range '1' '8' <|> repeat (range '1' '8' <> range '1' '8' <> range '1' '8'))))) \"18\"" $ match lexer2 "18" `shouldBe` MATCH "18"
         it "match (compile (repeat (mempty <|> range '1' '8' <|> range '1' '8' <> range '1' '8' <|> repeat (range '1' '8' <> range '1' '8' <> range '1' '8'))))) \"1881\"" $ match lexer2 "1881" `shouldBe` MATCH "1881"
  where
      lexer1 = makeLexer [TokenDefinition (repeat (mempty <|> atom 'a' <|> atoms "aa" <|> repeat (atoms "aaa"))) id]
      lexer2 = makeLexer [TokenDefinition (repeat (mempty <|> range '1' '8' <|> range '1' '8' <> range '1' '8' <|> repeat (range '1' '8' <> range '1' '8' <> range '1' '8'))) id]
