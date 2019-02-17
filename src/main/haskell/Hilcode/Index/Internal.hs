{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Hilcode.Index.Internal where

import Hilcode.Offset

import Hilcode.Prelude

import Text.Printf

newtype Index = Index Int
                  deriving (Eq, Ord, Enum)

makeIndex ∷ Int → Index
makeIndex index
  | index < 0 = error "Invalid index; must be non negative."
  | otherwise = Index index

instance Zero Index where
        zero = Index 0

instance Show Index where
        show (Index index) = show index

addOffset ∷ Index → Offset → Index
addOffset (Index index) offset = makeIndex <| withOffset offset (index +)

instance PrintfArg Index where
        formatArg (Index index) = formatInt index

instance HasGet [] Index where
        (!) list (Index index) = list ! index

instance HasGet Seq Index where
        (!) sequence (Index index) = sequence ! index

instance HasGet Vector Index where
        (!) vector (Index index) = vector ! index

instance HasGet IntMap Index where
        (!) map (Index index) = map ! index

instance HasPut (IntMap value) Index value where
        put (Index index) = put index
