{-# LANGUAGE DataKinds, KindSignatures #-}

module Main where

import PitchSetTheory
import Data.Group
import GHC.TypeLits

main = do
  print $ take 15 $ (generated :: [Dihedral 5])
  genChroma

-- | Generate some pieces with pitches from ch
genChroma = do

  let p = [0,1,2,3]
  print $ "Piece 1 "++(show p)
  print $ map (+60) $ 
     (   t 10 p
      ++ ti 6 p
      ++ t 4 p
      ++ ti 7 p
      ++ ti 8 p
      ++ t 4 p)

  let p = [0,4,7]
  print $ "Piece 2 "++ (show p)
  print $ 
    (map (+60) $ 
       (   t 3 p
        ++ ti 6 p
        ++ t 4 p
        ++ ti 11 p
        ++ ti 8 p)) ++
    (map (+65) $
       (   t 3 p
        ++ ti 6 p
        ++ t 1 p
        ++ ti 8 p
        ++ ti 1 p))
       

  let p = [0,1,7,8]
  print $ "Piece 3 "++ (show p)
  print $ 
    (map (+60) $ 
       (   t 3 p
        ++ ti 6 p
        ++ t 4 p
        ++ ti 11 p
        ++ ti 8 p)) ++
    (map (+65) $
       (   t 3 p
        ++ ti 6 p
        ++ t 1 p
        ++ ti 8 p
        ++ ti 1 p)) ++
    (map (+57) $ 
       (   t 3 p
        ++ ti 6 p
        ++ t 4 p
        ++ ti 11 p
        ++ ti 8 p))
       
