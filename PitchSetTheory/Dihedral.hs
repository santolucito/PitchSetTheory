{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PitchSetTheory.Dihedral where

import PitchSetTheory.PitchClass
import Data.Group

--I would love to be able to pull out the 12 to a type level literal
--so that I can generalize to Dihedral N
data Dihedral_12 = Dihedral_12
  { r :: Int  -- how many times to rotate
  , s :: Bool -- flip or not
  } deriving Show

instance Eq Dihedral_12 where
  x == y = 
    (s x == s y) &&
    ((r x == r y) || 
     (r x == (12-(r y)))
    )

instance Semigroup Dihedral_12 where
  x <> y = 
    Dihedral_12 
      { r = (r x + r y) `mod` 12
      , s = s x /= s y --xor
      }

instance Monoid Dihedral_12 where
  mempty = Dihedral_12 {r=0,s=False}
  mappend = (<>)

instance Group Dihedral_12 where
 invert x = 
   Dihedral_12 
     { r = 12-(r x)
     , s = s x --to invert flipped, flip back, otherwise leave it alone
     } 

instance Abelian Dihedral_12

class Group a => Cyclic a where
  generator :: a

-- D12 is not actually cyclic, but it is dicyclic
instance Cyclic Dihedral_12 where
  generator = Dihedral_12 {r = 1, s = True}

generate :: forall a. (Cyclic a) => [a]
generate =
  iterate (mappend (generator::a)) mempty

applyOpP:: Dihedral_12 -> PitchClass -> PitchClass
applyOpP f =
  (mappend (r f)). (if s f then invert else id)

applyOp :: Dihedral_12 -> PitchSet -> PitchSet
applyOp f = map $ applyOpP f

t :: Int -> PitchSet -> PitchSet
t n = applyOp $ Dihedral_12 { r = n, s = False }

ti :: Int -> PitchSet -> PitchSet 
ti n = applyOp $ Dihedral_12 { r = n, s = True }



