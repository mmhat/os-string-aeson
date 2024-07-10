{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module System.OsString.Aeson.Types where

import Data.Kind (Type)
import Data.Typeable (Typeable)
import System.IO (TextEncoding, utf16le, utf8)

newtype As (t :: k) a = As {unAs :: a}
    deriving (Eq, Foldable, Functor, Show, Traversable)

data Level
    = Nested
    | TopLevel

data Tag :: Level -> Type where
    Binary :: Tag a
    Text :: (enc :: Type) -> Tag a
    Tagged :: Tag 'Nested -> Tag 'TopLevel

pattern AsBinary
    :: forall {l :: Level} a
     . a -> As ('Binary :: Tag l) a
pattern AsBinary x = As x
{-# COMPLETE AsBinary #-}

pattern AsText
    :: forall {l :: Level} (enc :: Type) a
     . a -> As ('Text enc :: Tag l) a
pattern AsText x = As x
{-# COMPLETE AsText #-}

pattern AsTaggedBinary
    :: forall a
     . a -> As ('Tagged 'Binary :: Tag 'TopLevel) a
pattern AsTaggedBinary x = As x
{-# COMPLETE AsTaggedBinary #-}

pattern AsTaggedText
    :: forall (enc :: Type) a
     . a -> As ('Tagged ('Text enc) :: Tag 'TopLevel) a
pattern AsTaggedText x = As x
{-# COMPLETE AsTaggedText #-}

asBinary :: forall a. As 'Binary a -> a
asBinary (AsBinary x) = x
{-# INLINE asBinary #-}

asText :: forall (enc :: Type) a. As ('Text enc) a -> a
asText (AsText x) = x
{-# INLINE asText #-}

asTaggedBinary :: forall a. As ('Tagged 'Binary) a -> a
asTaggedBinary (AsTaggedBinary x) = x
{-# INLINE asTaggedBinary #-}

asTaggedText :: forall (enc :: Type) a. As ('Tagged ('Text enc)) a -> a
asTaggedText (AsTaggedText x) = x
{-# INLINE asTaggedText #-}

type family TagEncoding (t :: Tag l) :: Type where
    TagEncoding 'Binary = ()
    TagEncoding ('Text enc) = enc
    TagEncoding ('Tagged t') = TagEncoding t'

class (Typeable a) => IsTextEncoding a where
    textEncoding :: TextEncoding

data Utf8

instance IsTextEncoding Utf8 where
    textEncoding = utf8
    {-# INLINE textEncoding #-}

data Utf16LE

instance IsTextEncoding Utf16LE where
    textEncoding = utf16le
    {-# INLINE textEncoding #-}
