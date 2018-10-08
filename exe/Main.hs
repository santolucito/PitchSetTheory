module Main where

import PitchSetTheory

main = do
  print $ take 15 $ (generate :: [Dihedral_12])

{- 
allOps = let
    genOps op name = map (\i -> (name++(show i),op i, map toPitchClass (op i))) [0..11]
  in
    genOps tP "t-" ++ genOps tiP "ti-"
-}
