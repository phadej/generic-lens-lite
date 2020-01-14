{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
-- | Derive record field lenses generically.
--
-- == Usage with labels in "Optics.Label"
--
-- "Optics.Label" provides 'Optics.Label.LabelOptic' class which powers
-- the @OverloadedLabels@ instance for optics type.
--
-- It's possible to use 'L.HasField' and 'field' to derive instances
-- generically. If we have a simple record type which has 'GHC.Generics.Generic'
-- instance:
--
-- @
-- data Ex = Ex { exA :: Int, exB :: Char } deriving (Generic)
-- @
--
-- We can derive 'Optics.Label.LabelOptic' instances for all its fields
-- at once:
--
-- @
-- instance ('HasField' name Ex a, a ~ b) => LabelOptic name A_Lens Ex Ex a b where
--     labelOptic = 'field' @name
-- @
--
-- /Note:/ GHC will ask you to enable a lot of extensions, do it.
-- Youn need to enable @UndecidableInstances@ in particular to make
-- @FunctionalDependencies@ check pass.
--
module Data.Generics.Optics.Lite (
    field,
    L.HasField,
    ) where

import Data.Kind    (Type)
import GHC.TypeLits (Symbol)
import Optics.Core  (Lens', lensVL)

import qualified Data.Generics.Lens.Lite as L

-------------------------------------------------------------------------------
-- Public API
-------------------------------------------------------------------------------


-- | A lens that focuses on a field with a given name.
-- Compatible with the optics package's 'Optics.Lens.Lens' type.
--
-- __Note:__ the lens is /simple/, i.e. doesn't allow type-changing updates.
-- This keeps the implementation small and quick.
--
-- You also may want to specify
-- @
-- {-\# OPTIONS_GHC -funfolding-keeness-factor=100 #-} (or some other arbitrarily large number)
-- @
-- for GHC to inline more aggressively.
--
field
    :: forall (name :: Symbol) (r :: Type) (a :: Type). L.HasField name r a
    => Lens' r a
field = lensVL (L.field @name)
