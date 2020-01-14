{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -funfolding-keeness-factor=100 #-}
module Main (module Main) where

import Data.Generics.Lens.Lite (field)
import GHC.Generics            (Generic)

main :: IO ()
main = return ()

-------------------------------------------------------------------------------
-- Big record
-------------------------------------------------------------------------------

data Ex = Ex
    { exA :: Int
    , exB :: Bool
    , ex0 :: Char
    , ex1 :: Char
    , ex2 :: Char
    , ex3 :: Char
    , ex4 :: Char
    , ex5 :: Char
    , ex6 :: Char
    , ex7 :: Char
    , ex8 :: Char
    , ex9 :: Char
    } deriving (Generic)

_exA :: Functor f => (Int -> f Int) -> Ex -> f Ex
_exA = field @"exA"

_exB :: Functor f => (Bool -> f Bool) -> Ex -> f Ex
_exB = field @"exB"

_ex4 :: Functor f => (Char -> f Char) -> Ex -> f Ex
_ex4 = field @"ex4"

-------------------------------------------------------------------------------
-- Sum of records
-------------------------------------------------------------------------------

data Ex2
    = Ex2A { exC :: Char }
    | Ex2B { exC :: Char }
  deriving (Generic)

_exC :: Functor f => (Char -> f Char) -> Ex2 -> f Ex2
_exC = field @"exC"
