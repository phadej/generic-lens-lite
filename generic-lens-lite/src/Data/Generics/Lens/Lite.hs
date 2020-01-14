{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE EmptyCase              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
-- | Derive record field lenses generically.
module Data.Generics.Lens.Lite (
    field,
    HasField,
    ) where

import Data.Functor.Confusing (LensLike, Yoneda, fusing)
import Data.Kind              (Constraint, Type)
import Data.Proxy             (Proxy (..))
import GHC.TypeLits           (ErrorMessage (..), Symbol, TypeError)

import GHC.Generics

-------------------------------------------------------------------------------
-- Public API
-------------------------------------------------------------------------------

-- | Type-class restricting 'field' usage.
class HasField (name :: Symbol) r a | name r -> a where
    field__ :: Proxy name -> LensLikeYoneda' f r a

class HasFieldInternal (name :: Symbol) r a | name r -> a where
    field_ :: Proxy name -> LensLikeYoneda' f r a

-- | A lens that focuses on a field with a given name.
-- Compatible with the lens package's 'Control.Lens.Lens' type.
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
    :: forall (name :: Symbol) (r :: Type) (a :: Type) (f :: Type -> Type). (HasField name r a, Functor f)
    => (a -> f a) -> r -> f r
field = fusing (field__ (Proxy :: Proxy name))

instance HasFieldInternal name r a => HasField name r a where
    field__ = field_
    {-# INLINE field__ #-}

instance
    ( Generic r
    , ErrorCheck name r a (HasFieldPred name (Rep r))
    , HasFieldPred name (Rep r) ~ 'Just a
    , GField name (Rep r) a
    ) => HasFieldInternal name r a
  where
    field_ pname f s = fmap to (gfield pname f (from s))
    {-# INLINE field_ #-}

-------------------------------------------------------------------------------
-- Errors
-------------------------------------------------------------------------------

type family ErrorCheck (name :: Symbol) r a (res :: Maybe Type) :: Constraint where
    ErrorCheck _    _ _ ('Just _) = ()
    ErrorCheck name r a 'Nothing  = TypeError
      ( 'Text "Type " ':<>: 'ShowType r
      ':<>: 'Text " doesn't have field named " ':<>: 'Text name
      )

-- this prevents expansion of HasField "alias".
data Void1 a

instance {-# OVERLAPPING #-} HasField name (Void1 a) a where
    field__ _ _ n = case n of {}

-------------------------------------------------------------------------------
-- Generics
-------------------------------------------------------------------------------

type LensLikeYoneda' f r a = LensLike (Yoneda f) r r a a

class (HasFieldPred name f ~ 'Just a) => GField (name :: Symbol) f a | name f -> a where
    gfield :: Proxy name -> LensLikeYoneda' h (f ()) a

instance (GFieldSum name f a, i ~ D, HasFieldPred name f ~ 'Just a) => GField name (M1 i c f) a where
    gfield pname f (M1 x) = fmap M1 (gfieldsum pname f x)
    {-# INLINE gfield #-}

class HasFieldPred name f ~ 'Just a => GFieldSum (name :: Symbol) f a | name f -> a where
    gfieldsum :: Proxy name -> LensLikeYoneda' h (f ()) a

instance (HasFieldPred name (f :+: g) ~ 'Just a, GFieldSum name f a, GFieldSum name g a) => GFieldSum name (f :+: g) a where
    gfieldsum pname f (L1 x) = fmap L1 (gfieldsum pname f x)
    gfieldsum pname f (R1 y) = fmap R1 (gfieldsum pname f y)
    {-# INLINE gfieldsum #-}

instance (GFieldProd name f a, i ~ C, HasFieldPred name f ~ 'Just a) => GFieldSum name (M1 i c f) a where
    gfieldsum pname f (M1 x)  = fmap M1 (gfieldprod pname f x)
    {-# INLINE gfieldsum #-}

class (HasFieldPred name f ~ 'Just a) => GFieldProd (name :: Symbol) f a | name f -> a where
    gfieldprod :: Proxy name -> LensLikeYoneda' h (f ()) a

instance (c ~ 'MetaSel ('Just name) u s l, f ~ Rec0 a, i ~ S) => GFieldProd name (M1 i c f) a where
    gfieldprod _ f (M1 (K1 x)) = fmap (M1 . K1) (f x)
    {-# INLINE gfieldprod #-}

instance GFieldProd' name f g (HasFieldPred name f) a => GFieldProd name (f :*: g) a where
    gfieldprod = gfieldprod' (Proxy :: Proxy (HasFieldPred name f))
    {-# INLINE gfieldprod #-}

class (HasFieldPred name (f :*: g) ~ 'Just a) => GFieldProd' (name :: Symbol) f g (res :: Maybe Type) a where
    gfieldprod' :: Proxy res -> Proxy name ->  LensLikeYoneda' h ((f :*: g) ()) a

instance (a ~ a', GFieldProd name f a', HasFieldPred name (f :*: g) ~ 'Just a) => GFieldProd' name f g ('Just a') a where
    gfieldprod' _ pname f (x :*: y) = fmap (:*: y) (gfieldprod pname f x)
    {-# INLINE gfieldprod' #-}

instance (a ~ a', GFieldProd name g a', HasFieldPred name (f :*: g) ~ 'Just a) => GFieldProd' name f g 'Nothing a where
    gfieldprod' _ pname f (x :*: y) = fmap (x :*:) (gfieldprod pname f y)
    {-# INLINE gfieldprod' #-}

-------------------------------------------------------------------------------
-- TotalField
-------------------------------------------------------------------------------

type family Both (m1 :: Maybe Type) (m2 :: Maybe Type) :: Maybe Type where
    Both ('Just a) ('Just a) = 'Just a

type family Alt (m1 :: Maybe Type) (m2 :: Maybe Type) :: Maybe Type where
    Alt ('Just a) _ = 'Just a
    Alt _ b = b

type family HasFieldPred (field :: Symbol) f :: Maybe Type where
    HasFieldPred field (S1 ('MetaSel ('Just field) _ _ _) (Rec0 t)) =
        'Just t
    HasFieldPred field (S1 _ _)  = 'Nothing
    HasFieldPred field (l :*: r) = Alt (HasFieldPred field l) (HasFieldPred field r)
    HasFieldPred field (l :+: r) = Both (HasFieldPred field l) (HasFieldPred field r)
    HasFieldPred field (C1 _ f)  = HasFieldPred field f
    HasFieldPred field (D1 _ f)  = HasFieldPred field f
    HasFieldPred field (K1 _ _)  = 'Nothing
    HasFieldPred field U1        = 'Nothing
    HasFieldPred field V1        = 'Nothing
