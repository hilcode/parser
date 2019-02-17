{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Hilcode.RegExp.Internal where

import           Data.EnumSet
                 ( EnumSet
                 )
import qualified Data.EnumSet
import qualified Data.List.Index
import           Data.Sequence
                 ( Seq((:<|), (:|>))
                 , (><)
                 )
import qualified Data.Vector

import Hilcode.Index
import Hilcode.Offset

import Hilcode.Prelude

data Command atom token = Atom !atom !(Vector Index)
                        | Range !atom !atom !(Vector Index)
                        | Success !SuccessId !(MakeToken atom token)
                            deriving (Eq, Ord, Show)

data Program atom token = Program !(Vector Index) !(Vector (Command atom token))
                            deriving (Show)

newtype MakeToken atom token = MakeToken ([atom] → token)

instance Eq (MakeToken atom token) where
        _ == _ = True

instance Ord (MakeToken atom token) where
        _ `compare` _ = EQ

instance Show (MakeToken atom token) where
        show _ = "MakeToken"

newtype SuccessId = SuccessId Int
                      deriving (Eq, Ord, Show)

increment ∷ SuccessId → SuccessId
increment (SuccessId successId) = SuccessId (successId + 1)

instance Zero SuccessId where
        zero = SuccessId 0

data Step atom token = StepAtom !atom
                     | StepRange !atom !atom
                     | StepJump !Offset
                     | StepFork !Offset
                     | StepSuccess !SuccessId !(MakeToken atom token)
                         deriving (Eq, Ord, Show)

data MatcherStep atom token = MatcherStepAtom !atom
                            | MatcherStepRange !atom !atom
                            | MatcherSuccess !SuccessId !(MakeToken atom token)
                                deriving (Eq, Ord)

data InitialProgram atom token = InitialProgram !(Vector Index) !(Vector (Maybe (Command atom token)))

data LoopResult = Continue
                | Stop

data MatchResult atom = MATCH [atom]
                      | NO_MATCH
                          deriving (Eq, Show)

data StepState = StepState (EnumSet Index) (EnumSet Index)

newtype RegExp atom token = RegExp (Seq (Step atom token))
                              deriving (Eq, Show)

instance Semigroup (RegExp atom token) where
        (<>) (RegExp lft) (RegExp rgt) = RegExp (lft <> rgt)

instance Monoid (RegExp atom token) where
        mempty = RegExp empty

emptyStepState ∷ StepState
emptyStepState = StepState empty empty

resultIndices ∷ StepState → EnumSet Index
resultIndices (StepState _ indices) = indices

markIndexAsSeen ∷ Index → StepState → StepState
markIndexAsSeen index (StepState seen result) = StepState (add index seen) result

markIndex ∷ Index → StepState → StepState
markIndex index (StepState seen result) = StepState (add index seen) (add index result)

contains ∷ StepState → Index → Bool
contains (StepState seen _) index = index `isMemberOf` seen

atom ∷ atom → RegExp atom token
atom ch = RegExp (singleton (StepAtom ch))

range ∷ Ord atom ⇒ atom → atom → RegExp atom token
range lowerBound upperBound
  | lowerBound < upperBound = RegExp (singleton (StepRange lowerBound upperBound))
  | lowerBound == upperBound = atom lowerBound
  | otherwise = error "Invalid bounds: upper bound must be greater than or equal to lower bound."

atoms ∷ Foldable foldable ⇒ foldable atom → RegExp atom token
atoms chs = RegExp <| foldl' (\ steps ch -> steps :|> StepAtom ch) empty chs

repeat ∷ RegExp atom token → RegExp atom token
repeat (RegExp steps) = RegExp (stepFork steps :<| (steps :|> stepJumpBackward steps))

(<|>) ∷ RegExp atom token → RegExp atom token → RegExp atom token
(<|>) (RegExp lftSteps) (RegExp rgtSteps) = RegExp (stepFork lftSteps :<| lftSteps >< (stepJumpForward rgtSteps :<| rgtSteps))

stepFork ∷ HasSize steps ⇒ steps (Step atom token) → Step atom token
stepFork steps = size steps + 2 |> makeOffset |> StepFork

stepJumpForward ∷ HasSize steps ⇒ steps (Step atom token) → Step atom token
stepJumpForward steps = size steps + 1 |> makeOffset |> StepJump

stepJumpBackward ∷ HasSize steps ⇒ steps (Step atom token) → Step atom token
stepJumpBackward steps = (-(size steps + 1)) |> makeOffset |> StepJump

compile ∷ forall atom token . Ord atom ⇒ RegExp atom token → Program atom token
compile (RegExp steps') = simplify $ InitialProgram startIndices commands
  where
      steps ∷ Vector (Step atom token)
      steps = fromList <| toList steps'

      startIndices ∷ Vector Index
      startIndices = go zero emptyStepState |> resultIndices |> toList |> fromList

      commands ∷ Vector (Maybe (Command atom token))
      commands = fmap (removeJumps .> fmap toCommand) (isteps <| steps)

      removeJumps ∷ (Index, Step atom token) → Maybe (MatcherStep atom token, StepState)
      removeJumps (index, step)
        = case step of
              StepAtom ch                     -> Just (MatcherStepAtom ch, go (succ index) emptyStepState)
              StepRange lower upper           -> Just (MatcherStepRange lower upper, go (succ index) emptyStepState)
              StepSuccess successId makeToken -> Just (MatcherSuccess successId makeToken, go (succ index) emptyStepState)
              StepJump _                      -> Nothing
              StepFork _                      -> Nothing

      toCommand ∷ (MatcherStep atom token, StepState) → Command atom token
      toCommand (MatcherStepAtom ch, stepState)           = Atom ch (fromList (toList (resultIndices stepState)))
      toCommand (MatcherStepRange lower upper, stepState) = Range lower upper (fromList (toList (resultIndices stepState)))
      toCommand (MatcherSuccess successId makeToken, _)   = Success successId makeToken

      isSuccessIndex ∷ Index → Bool
      isSuccessIndex index
        = case steps' ! index of
              StepSuccess _ _ -> True
              _               -> False

      go ∷ Index → StepState → StepState
      go index currentStepState
        | isSuccessIndex index = markIndex index currentStepState
        | currentStepState `contains` index = currentStepState
        | otherwise =
          case steps ! index of
              StepAtom _       -> markIndex index currentStepState
              StepRange _ _    -> markIndex index currentStepState
              StepSuccess _ _  -> markIndex index currentStepState
              StepJump offset' -> go (addOffset index offset') (markIndexAsSeen index currentStepState)
              StepFork offset' -> let result' = go (succ index) (markIndexAsSeen index currentStepState) in go (addOffset index offset') result'

simplify ∷ forall atom token . Ord atom ⇒ InitialProgram atom token → Program atom token
simplify initialProgram = simplify' initialProgram |> toProgram
  where
      reindexCommand ∷ IntMap Index → Command atom token → Command atom token
      reindexCommand map (Atom ch indices)           = Atom ch (reindex map indices)
      reindexCommand map (Range lower upper indices) = Range lower upper (reindex map indices)
      reindexCommand _ success@(Success _ _)         = success

      simplify' ∷ InitialProgram atom token → InitialProgram atom token
      simplify' (InitialProgram startIndices commands) = loop simplifyFurther betterInitialProgram
        where
            map ∷ IntMap Index
            map = indexMap (toList commands)

            betterInitialProgram ∷ InitialProgram atom token
            betterInitialProgram = InitialProgram (reindex map startIndices) (reduce (reindexCommand map .> Just) commands)

      simplifyFurther ∷ InitialProgram atom token → (LoopResult, InitialProgram atom token)
      simplifyFurther initialProgram@(InitialProgram _ commands)
        | size map' == size commands = (Stop, initialProgram)
        | otherwise = (Continue, fixInitialProgram initialProgram)
        where
            map' ∷ Map (Command atom token) Index
            map' = firstCommandMap (toList commands)

            fixInitialProgram ∷ InitialProgram atom token → InitialProgram atom token
            fixInitialProgram (InitialProgram startIndices commands) = InitialProgram (reindex map startIndices) (reduce (reindexCommand map .> Just) commands')
              where
                  fix ∷ ((Index, Index, [Maybe (Command atom token)]), ([Maybe (Command atom token)], IntMap Index)) → ((Index, Index, [Maybe (Command atom token)]), ([Maybe (Command atom token)], IntMap Index))
                  fix ((index, actualIndex, Nothing : rest), (commands', map)) = fix ((succ index, actualIndex, rest), (Nothing : commands', map))
                  fix ((index, actualIndex, command@(Just command') : rest), (commands', map))
                    = let firstIndex = map' ! command'
                          nextActualIndex = if firstIndex == index then succ actualIndex else actualIndex
                          targetIndex = if firstIndex == index then actualIndex else firstIndex
                          command'' = if firstIndex == index then command else Nothing
                        in fix ((succ index, nextActualIndex, rest), (command'' : commands', put index targetIndex map))
                  fix ((index, actualIndex, []), (commands', map)) = ((index, actualIndex, []), (reverse commands', put index actualIndex map))

                  simplification ∷ ([Maybe (Command atom token)], IntMap Index)
                  simplification = snd <| fix ((zero, zero, toList commands), ([], empty))

                  map ∷ IntMap Index
                  map = snd simplification

                  commands' ∷ Vector (Maybe (Command atom token))
                  commands' = fromList <| fst simplification

      toProgram ∷ InitialProgram atom token → Program atom token
      toProgram (InitialProgram _startIndices _commands) = Program _startIndices (reduce id _commands)

reduce ∷ forall a b . (a → b) → Vector (Maybe a) → Vector b
reduce f = Data.Vector.concatMap maybeMap
  where
      maybeMap ∷ Maybe a → Vector b
      maybeMap Nothing  = empty
      maybeMap (Just a) = singleton (f a)

loop ∷ (a → (LoopResult, a)) → a → a
loop f a
  = case f a of
        (Continue, a') -> loop f a'
        (Stop, a')     -> a'

reindex ∷ IntMap Index → Vector Index → Vector Index
reindex map indices = reindex' map indices |> toList |> fromList

reindex' ∷ IntMap Index → Vector Index → Set Index
reindex' map is = foldl' (flip add) empty ((map !) `fmap` is)

indexMap ∷ forall atom token . [Maybe (Command atom token)] → IntMap Index
indexMap commands
  = case makeIndexMap ((zero, zero), commands, empty) of
        ((index, actualIndex), _, map) -> put index actualIndex map
  where
      makeIndexMap ∷ ((Index, Index), [Maybe (Command atom token)], IntMap Index) → ((Index, Index), [Maybe (Command atom token)], IntMap Index)
      makeIndexMap ((index, actualIndex), command : rest, map)
        = case command of
              Nothing       -> makeIndexMap ((succ index, actualIndex), rest, map)
              Just _command -> makeIndexMap ((succ index, succ actualIndex), rest, put index actualIndex map)
      makeIndexMap result = result

firstCommandMap ∷ forall atom token . Ord atom ⇒ [Maybe (Command atom token)] → Map (Command atom token) Index
firstCommandMap commands
  = case makeFirstCommandMap (zero, commands, empty) of
        (_, _, map) -> map
  where
      makeFirstCommandMap ∷ (Index, [Maybe (Command atom token)], Map (Command atom token) Index) → (Index, [Maybe (Command atom token)], Map (Command atom token) Index)
      makeFirstCommandMap (index, Nothing : rest, map) = makeFirstCommandMap (succ index, rest, map)
      makeFirstCommandMap (index, Just command : rest, map)
        | member command map = makeFirstCommandMap (succ index, rest, map)
        | otherwise = makeFirstCommandMap (succ index, rest, put command index map)
      makeFirstCommandMap result = result

match ∷ forall atom token . Ord atom ⇒ Lexer atom token → [atom] → MatchResult atom
match (Lexer (Program startIndices commands')) input
  = case go (fromList (toList startIndices), input, []) of
        (endIndices, [], atoms) -> if success `isMemberOf` endIndices then MATCH <| reverse atoms else NO_MATCH
        _                       -> NO_MATCH
  where
      commands ∷ Seq (Command atom token)
      commands = fromList (toList commands')

      success ∷ Index
      success = makeIndex <| size commands - 1

      go ∷ (EnumSet Index, [atom], [atom]) → (EnumSet Index, [atom], [atom])
      go result@(_, [], _) = result
      go result@(threads, ch : rest, atoms)
        | isEmpty threads = result
        | otherwise = go (stepThreads commands ch (Data.EnumSet.delete success threads), rest, ch : atoms)

stepThread ∷ Ord atom ⇒ Seq (Command atom token) → atom → Index → EnumSet Index
stepThread commands ch index
  = case commands ! index of
        Atom valid indices        -> if ch == valid then fromList (toList indices) else empty
        Range lower upper indices -> if lower <= ch && ch <= upper then fromList (toList indices) else empty
        Success _ _               -> empty

stepThreads ∷ Ord atom ⇒ Seq (Command atom token) → atom → EnumSet Index → EnumSet Index
stepThreads commands ch indices = foldMap (stepThread commands ch) (toList indices)

isteps ∷ Vector (Step atom token) → Vector (Index, Step atom token)
isteps steps = mapFst makeIndex <$> fromList (Data.List.Index.indexed <| toList steps)

newtype Lexer atom token = Lexer (Program atom token)
                             deriving (Show)

data TokenDefinition atom token = TokenDefinition (RegExp atom token) ([atom] → token)

makeLexer ∷ forall atom token . Ord atom ⇒ [TokenDefinition atom token] → Lexer atom token
makeLexer tokenizers = foldl' (<>) mempty regExps |> compile |> Lexer
  where
      regExps ∷ [RegExp atom token]
      regExps = mapWithState zero tokenizers makeStepSuccess

      makeStepSuccess ∷ TokenDefinition atom token → SuccessId → (SuccessId, RegExp atom token)
      makeStepSuccess (TokenDefinition (RegExp steps) f) successId = (increment successId, RegExp (steps :|> StepSuccess (SuccessId 0) (MakeToken f)))

mapWithState ∷ s → [x] → (x → s → (s, y)) → [y]
mapWithState _ [] _ = []
mapWithState s (x : xs) f = y : mapWithState s' xs f
  where
      (s', y) = f x s
