{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -funfolding-keeness-factor=100 #-}
module Main (module Main) where

import Data.Generics.Optics.Lite (HasField, field)
import GHC.Generics              (Generic)
import Optics.Core               (A_Lens, LabelOptic (..), Lens')

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

instance (HasField name Ex a, a ~ b) => LabelOptic name A_Lens Ex Ex a b where
    labelOptic = field @name

_exA :: Lens' Ex Int
_exA = field @"exA"

_exA' :: Lens' Ex Int
_exA' = #exA

_exB :: Lens' Ex Bool
_exB = field @"exB"

_ex4 :: Lens' Ex Char
_ex4 = field @"ex4"

-------------------------------------------------------------------------------
-- Sum of records
-------------------------------------------------------------------------------

data Ex2
    = Ex2A { exC :: Char }
    | Ex2B { exC :: Char }
  deriving (Generic)

_exC :: Lens' Ex2 Char
_exC = field @"exC"
