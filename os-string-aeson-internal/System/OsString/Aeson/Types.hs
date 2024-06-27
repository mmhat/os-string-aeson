{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveTraversable #-}

module System.OsString.Aeson.Types where

import System.IO (TextEncoding, utf16le, utf8)

newtype AsBinary a = AsBinary {asBinary :: a}
    deriving (Eq, Foldable, Functor, Show, Traversable)

newtype AsText encoding a = AsText {asText :: a}
    deriving (Eq, Foldable, Functor, Show, Traversable)

newtype Tagged a = Tagged {tagged :: a}
    deriving (Eq, Foldable, Functor, Show, Traversable)

class IsTextEncoding a where
    textEncoding :: TextEncoding

data Utf8

instance IsTextEncoding Utf8 where
    textEncoding = utf8
    {-# INLINE textEncoding #-}

data Utf16LE

instance IsTextEncoding Utf16LE where
    textEncoding = utf16le
    {-# INLINE textEncoding #-}
