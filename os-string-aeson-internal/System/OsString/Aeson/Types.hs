{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module System.OsString.Aeson.Types where

import Control.Newtype (Newtype (..))
import Data.Kind (Type)
import Data.Typeable (Typeable)
import System.IO (TextEncoding, utf16le, utf8)

type family EncodingTag a :: Type

newtype AsBinary a = AsBinary {asBinary :: a}
    deriving (Eq, Foldable, Functor, Show, Traversable)

instance Newtype (AsBinary a) a

type instance EncodingTag (AsBinary a) = ()

newtype AsText encoding a = AsText {asText :: a}
    deriving (Eq, Foldable, Functor, Show, Traversable)

instance Newtype (AsText encoding a) a

type instance EncodingTag (AsText encoding a) = encoding

newtype Tagged a = Tagged {tagged :: a}
    deriving (Eq, Foldable, Functor, Show, Traversable)

instance Newtype (Tagged a) a

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
