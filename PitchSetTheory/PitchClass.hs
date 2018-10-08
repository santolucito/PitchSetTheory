{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}

module PitchSetTheory.PitchClass where

import Data.Group

type PitchClass = Int
--newtype PitchClass = PitchSet [Int]

instance Semigroup PitchClass where
  x <> y = (x + y) `mod` 12

instance Monoid PitchClass where
  mempty = 0 :: Int
  mappend = (<>) 

instance Group PitchClass where
  invert p = 12 - p

instance Abelian PitchClass

type PitchSet = [PitchClass]

toPitchClass = \case
  0 -> "C"
  1 -> "Db"
  2 -> "D"
  3 -> "Eb"
  4 -> "E"
  5 -> "F"
  6 -> "F#"
  7 -> "G"
  8 -> "Ab"
  9 -> "A"
  10 -> "Bb"
  11 -> "B"
  otherwise -> undefined
