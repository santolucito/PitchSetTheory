{-# LANGUAGE TypeSynonymInstances #-}
-- this is only need for the definition of cyclic, once my change is accepted to Data.Groups this is not needed
-- {-# LANGUAGE ScopedTypeVariables #-} 
{-# LANGUAGE DataKinds, KindSignatures #-}

module PitchSetTheory.Dihedral where

import PitchSetTheory.PitchClass
import Data.Group
import GHC.TypeLits

-- | The Dihedral n group captures the set of closed operations (transpose and transpose inverse) on a PitchSet
--   This definition is only useful if you are composing operations (eg in the Cyclic instance)
--   Applying single operation on a PitchSet is the same regardless of the group definition
data KnownNat n => Dihedral (n :: Nat) = Dihedral
  { r :: Integer
  , s :: Bool
  } deriving Show

-- seems wasteful that I should have to put the type context here if it is already avaible in the def of Dihedral...
instance KnownNat n => Eq (Dihedral n) where
  x == y = 
    (s x == s y) &&
    ((r x == r y) || 
     (r x == ((natVal x) -(r y)))
    )

instance KnownNat n => Semigroup (Dihedral n) where
  x <> y = 
    Dihedral
      { r = (r x + r y) `mod` (natVal x)
      , s = s x /= s y --xor
      } --do i need a type annotation here? guess not...

instance KnownNat n => Monoid (Dihedral n)where
  mempty = Dihedral {r=0,s=False}
  mappend = (<>)

instance KnownNat n => Group (Dihedral n) where
 invert x = 
   Dihedral
     { r = (natVal x) -(r x)
     , s = s x --to invert flipped, flip back, otherwise leave it alone
     } 

-- I need to check that the group is actually abelian (this class doesnt give us anything useful anyway)
instance KnownNat n => Abelian (Dihedral n)

-- this is only true for certain n
-- actually now i can properly express that
instance KnownNat n => Cyclic (Dihedral n) where
  generator = Dihedral {r = 1, s = True}

{-
I have put these in the Group package, but that change hasnt been accepted yet
class Group a => Cyclic a where
  generator :: a

generated :: forall a. (Cyclic a) => [a]
generated =
  iterate (mappend (generator::a)) mempty
-}

-- there is something fishy here about composing operations
-- somehow it seems that it will bypass the group composiiton strucutre of Dihedral
-- do i need a monad to control the composition? 
-- somehow i shouldnt allow a user to ...
-- does this really matter?
applyOpP:: Dihedral n -> PitchClass -> PitchClass
applyOpP f =
  (mappend (r f)). (if s f then invert else id)

applyOp :: Dihedral n -> PitchSet -> PitchSet
applyOp f = map $ applyOpP f

t :: Integer -> PitchSet -> PitchSet
t n = applyOp $ Dihedral { r = n, s = False }

ti :: Integer -> PitchSet -> PitchSet 
ti n = applyOp $ Dihedral { r = n, s = True }



