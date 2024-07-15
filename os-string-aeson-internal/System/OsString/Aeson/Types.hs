{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}

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
    Base64 :: Tag a
    Binary :: Tag a
    Text :: (enc :: Type) -> Tag a
    Tagged :: Tag 'Nested -> Tag 'TopLevel

#if MIN_TOOL_VERSION_ghc(9,0,0)
pattern AsBase64
    :: forall {l :: Level} a. a -> As ('Base64 :: Tag l) a
#else
pattern AsBase64
    :: forall (l :: Level) a. a -> As ('Base64 :: Tag l) a
#endif
pattern AsBase64 x = As x

{-# COMPLETE AsBase64 #-}

#if MIN_TOOL_VERSION_ghc(9,0,0)
pattern AsBinary
    :: forall {l :: Level} a. a -> As ('Binary :: Tag l) a
#else
pattern AsBinary
    :: forall (l :: Level) a. a -> As ('Binary :: Tag l) a
#endif
pattern AsBinary x = As x

{-# COMPLETE AsBinary #-}

#if MIN_TOOL_VERSION_ghc(9,0,0)
pattern AsText
    :: forall (enc :: Type) {l :: Level} a
     . a -> As ('Text enc :: Tag l) a
#else
pattern AsText
    :: forall (enc :: Type) (l :: Level) a
     . a -> As ('Text enc :: Tag l) a
#endif
pattern AsText x = As x

{-# COMPLETE AsText #-}

pattern AsTaggedBase64
    :: forall a. a -> As ('Tagged 'Base64 :: Tag 'TopLevel) a
pattern AsTaggedBase64 x = As x

{-# COMPLETE AsTaggedBase64 #-}

pattern AsTaggedBinary
    :: forall a. a -> As ('Tagged 'Binary :: Tag 'TopLevel) a
pattern AsTaggedBinary x = As x

{-# COMPLETE AsTaggedBinary #-}

pattern AsTaggedText
    :: forall (enc :: Type) a. a -> As ('Tagged ('Text enc) :: Tag 'TopLevel) a
pattern AsTaggedText x = As x

{-# COMPLETE AsTaggedText #-}

asBase64 :: forall a. As 'Base64 a -> a
asBase64 (AsBase64 x) = x
{-# INLINE asBase64 #-}

asBinary :: forall a. As 'Binary a -> a
asBinary (AsBinary x) = x
{-# INLINE asBinary #-}

asText :: forall (enc :: Type) a. As ('Text enc) a -> a
asText (AsText x) = x
{-# INLINE asText #-}

asTaggedBase64 :: forall a. As ('Tagged 'Base64) a -> a
asTaggedBase64 (AsTaggedBase64 x) = x
{-# INLINE asTaggedBase64 #-}

asTaggedBinary :: forall a. As ('Tagged 'Binary) a -> a
asTaggedBinary (AsTaggedBinary x) = x
{-# INLINE asTaggedBinary #-}

asTaggedText :: forall (enc :: Type) a. As ('Tagged ('Text enc)) a -> a
asTaggedText (AsTaggedText x) = x
{-# INLINE asTaggedText #-}

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
