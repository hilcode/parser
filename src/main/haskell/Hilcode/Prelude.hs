{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Hilcode.Prelude
    ( module Prelude
    , module Flow
    , module Control.Applicative
    , module Control.Monad
    , module Data.Bool
    , module Data.Char
    , module Data.Eq
    , module Data.Foldable
    , module Data.Function
    , module Data.Functor
    , module Data.Int
    , module Data.IntMap.Strict
    , module Data.List
    , module Data.Map.Strict
    , module Data.Maybe
    , module Data.Monoid
    , module Data.Ord
    , module Data.Semigroup
    , module Data.Sequence
    , module Data.Set
    , module Data.String
    , module Data.Traversable
    , module Data.Tuple
    , module Data.Vector
    , module System.IO
    , module Text.Show
    , mapFst
    , mapSnd
    , Zero
    , zero
    , IsEmpty
    , isEmpty
    , IsMemberOf
    , isMemberOf
    , HasEmpty
    , empty
    , HasSingleton
    , singleton
    , HasFromList
    , fromList
    , HasToList
    , toList
    , HasAdd
    , add
    , HasGet
    , (!)
    , HasKeys
    , keys
    , HasPut
    , put
    , HasSize
    , size
    ) where

import           Control.Applicative
                 ( Applicative
                 , pure
                 , (<*>)
                 )
import           Control.Monad
                 ( Monad
                 , (>>=)
                 , join
                 , sequence
                 )
import           Data.Bool
                 ( Bool(..)
                 , otherwise
                 , (&&)
                 , (||)
                 )
import           Data.Char
                 ( Char
                 , ord
                 , chr
                 )
import           Data.EnumSet
                 ( EnumSet
                 )
import qualified Data.EnumSet
import           Data.Eq
                 ( Eq
                 , (/=)
                 , (==)
                 )
import           Data.Foldable
                 ( Foldable
                 , concat
                 , concatMap
                 , foldMap
                 , foldl'
                 )
import qualified Data.Foldable
import           Data.Function
                 ( flip
                 , id
                 , ($)
                 , (.)
                 )
import           Data.Functor
                 ( Functor
                 , fmap
                 , (<$>)
                 )
import           Data.Int
                 ( Int
                 )
import           Data.IntMap.Strict
                 ( IntMap
                 )
import qualified Data.IntMap.Strict
import           Data.List
                 ( reverse
                 , zip
                 )
import qualified Data.List
import           Data.Map.Strict
                 ( Map
                 , member
                 )
import qualified Data.Map.Strict
import           Data.Maybe
                 ( Maybe(..)
                 )
import           Data.Monoid
                 ( Monoid
                 , mempty
                 )
import           Data.Ord
                 ( Ord
                 , Ordering(..)
                 , (<)
                 , (<=)
                 , (>)
                 , (>=)
                 , compare
                 , min
                 , max
                 )
import           Data.Semigroup
                 ( Semigroup
                 , (<>)
                 )
import           Data.Sequence
                 ( Seq
                 )
import qualified Data.Sequence
import           Data.Set
                 ( Set
                 )
import qualified Data.Set
import           Data.String
                 ( String
                 )
import           Data.Traversable
                 ( traverse
                 )
import           Data.Tuple
                 ( fst
                 , snd
                 )
import           Data.Vector
                 ( Vector
                 )
import qualified Data.Vector
import           Flow
import           Prelude
                 ( Enum
                 , succ
                 , pred
                 , error
                 , undefined
                 , (+)
                 , (-)
                 )
import           System.IO
                 ( IO
                 , print
                 )
import           Text.Show
                 ( Show
                 , show
                 )

mapFst ∷ (a → b) → (a, c) → (b, c)
mapFst f (a, c) = (f a, c)

mapSnd ∷ (b → c) → (a, b) → (a, c)
mapSnd f (a, b) = (a, f b)

class Zero a where
        zero :: a

class IsEmpty collection where
        isEmpty ∷ collection a → Bool

instance IsEmpty [] where
        isEmpty = Data.List.null

instance IsEmpty Seq where
        isEmpty = Data.Sequence.null

instance IsEmpty Vector where
        isEmpty = Data.Vector.null

instance IsEmpty IntMap where
        isEmpty = Data.IntMap.Strict.null

instance IsEmpty (Map key) where
        isEmpty = Data.Map.Strict.null

instance IsEmpty Set where
        isEmpty = Data.Set.null

instance IsEmpty EnumSet where
        isEmpty = Data.EnumSet.null

class IsMemberOf collection element where
        isMemberOf ∷ Eq element => element -> collection element -> Bool

instance IsMemberOf [] element where
        isMemberOf = Data.List.elem

instance IsMemberOf Seq element where
        isMemberOf = Data.List.elem

instance IsMemberOf Vector element where
        isMemberOf = Data.Vector.elem

instance Ord element => IsMemberOf Set element where
        isMemberOf = Data.Set.member

instance Enum element => IsMemberOf EnumSet element where
        isMemberOf = Data.EnumSet.member

class HasEmpty collection where
        empty ∷ collection a

instance HasEmpty [] where
        empty = []

instance HasEmpty Seq where
        empty = Data.Sequence.empty

instance HasEmpty Vector where
        empty = Data.Vector.empty

instance HasEmpty IntMap where
        empty = Data.IntMap.Strict.empty

instance HasEmpty (Map k) where
        empty = Data.Map.Strict.empty

instance HasEmpty Set where
        empty = Data.Set.empty

instance HasEmpty EnumSet where
        empty = Data.EnumSet.empty

class HasSingleton collection element where
        singleton ∷ element -> collection element

instance HasSingleton [] element where
        singleton a = [a]

instance HasSingleton Seq element where
        singleton = Data.Sequence.singleton

instance HasSingleton Vector element where
        singleton = Data.Vector.singleton

instance HasSingleton Set element where
        singleton = Data.Set.singleton

instance Enum element => HasSingleton EnumSet element where
        singleton = Data.EnumSet.singleton

class HasGet collection index where
        (!) ∷ collection a → index → a

instance HasGet [] Int where
        (!) = (Data.List.!!)

instance HasGet Seq Int where
        (!) = Data.Sequence.index

instance HasGet Vector Int where
        (!) = (Data.Vector.!)

instance HasGet IntMap Int where
        (!) = (Data.IntMap.Strict.!)

instance Ord k ⇒ HasGet (Map k) k where
        (!) = (Data.Map.Strict.!)

class HasAdd collection a where
        add ∷ a → collection a → collection a

instance HasAdd [] a where
        add a list = a : list

instance HasAdd Seq a where
        add = (Data.Sequence.<|)

instance Ord a ⇒ HasAdd Set a where
        add = Data.Set.insert

instance Enum a ⇒ HasAdd EnumSet a where
        add = Data.EnumSet.insert

class HasKeys collection key where
        keys ∷ collection → [key]

instance HasKeys (Map key value) key where
        keys = Data.Map.Strict.keys

instance HasKeys (IntMap value) Int where
        keys = Data.IntMap.Strict.keys

class HasPut collection key value where
        put ∷ key → value → collection → collection

instance HasPut (IntMap value) Int value where
        put = Data.IntMap.Strict.insert

instance Ord key ⇒ HasPut (Map key value) key value where
        put = Data.Map.Strict.insert

class HasFromList collection element where
        fromList ∷ [element] → collection element

instance HasFromList Seq element where
        fromList = Data.Sequence.fromList

instance HasFromList Vector element where
        fromList = Data.Vector.fromList

instance Ord element => HasFromList Set element where
        fromList = Data.Set.fromList

instance Enum element => HasFromList EnumSet element where
        fromList = Data.EnumSet.fromList

class HasToList collection element where
        toList ∷ collection element → [element]

instance HasToList [] element where
        toList = id

instance HasToList Seq element where
        toList = Data.Foldable.toList

instance HasToList Vector element where
        toList = Data.Vector.toList

instance HasToList Set element where
        toList = Data.Set.toList

instance Enum element => HasToList EnumSet element where
        toList = Data.EnumSet.toList

class HasSize collection where
        size ∷ collection a → Int

instance HasSize [] where
        size = Data.List.length

instance HasSize IntMap where
        size = Data.IntMap.Strict.size

instance HasSize (Map k) where
        size = Data.Map.Strict.size

instance HasSize Seq where
        size = Data.Sequence.length

instance HasSize Vector where
        size = Data.Vector.length
