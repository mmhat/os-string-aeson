{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}

module System.OsString.Aeson.Internal.Types where

import Data.Typeable (Typeable)
import System.IO (TextEncoding, utf16le, utf8)

newtype As (t :: k) a = As {unAs :: a}
    deriving (Eq, Foldable, Functor, Show, Traversable)

data Base64

data Binary

data Textual enc

data Tagged a

#if MIN_TOOL_VERSION_ghc(9,0,0)
pattern AsBase64 :: a -> As Base64 a
#else
pattern AsBase64 :: a -> As Base64 a
#endif
pattern AsBase64 x = As x

{-# COMPLETE AsBase64 #-}

#if MIN_TOOL_VERSION_ghc(9,0,0)
pattern AsBinary :: a -> As Binary a
#else
pattern AsBinary :: a -> As Binary a
#endif
pattern AsBinary x = As x

{-# COMPLETE AsBinary #-}

#if MIN_TOOL_VERSION_ghc(9,0,0)
pattern AsTextual :: forall enc a . a -> As (Textual enc) a
#else
pattern AsTextual :: forall enc a . a -> As (Textual enc) a
#endif
pattern AsTextual x = As x

{-# COMPLETE AsTextual #-}

pattern AsTaggedBase64 :: a -> As (Tagged Base64) a
pattern AsTaggedBase64 x = As x

{-# COMPLETE AsTaggedBase64 #-}

pattern AsTaggedBinary :: a -> As (Tagged Binary) a
pattern AsTaggedBinary x = As x

{-# COMPLETE AsTaggedBinary #-}

pattern AsTaggedTextual :: forall enc a. a -> As (Tagged (Textual enc)) a
pattern AsTaggedTextual x = As x

{-# COMPLETE AsTaggedTextual #-}

asBase64 :: As Base64 a -> a
asBase64 (AsBase64 x) = x
{-# INLINE asBase64 #-}

asBinary :: As Binary a -> a
asBinary (AsBinary x) = x
{-# INLINE asBinary #-}

asTextual :: forall enc a. As (Textual enc) a -> a
asTextual (AsTextual x) = x
{-# INLINE asTextual #-}

asTaggedBase64 :: As (Tagged Base64) a -> a
asTaggedBase64 (AsTaggedBase64 x) = x
{-# INLINE asTaggedBase64 #-}

asTaggedBinary :: As (Tagged Binary) a -> a
asTaggedBinary (AsTaggedBinary x) = x
{-# INLINE asTaggedBinary #-}

asTaggedTextual :: forall enc a. As (Tagged (Textual enc)) a -> a
asTaggedTextual (AsTaggedTextual x) = x
{-# INLINE asTaggedTextual #-}

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
