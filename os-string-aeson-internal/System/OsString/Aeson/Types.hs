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

newtype As (t :: Tag l) a = As {unAs :: a}
    deriving (Eq, Foldable, Functor, Show, Traversable)

data Level
    = Nested
    | TopLevel

data Tag :: Level -> Type where
    Binary :: Tag a
    Text :: (enc :: Type) -> Tag a
    Tagged :: Tag 'Nested -> Tag 'TopLevel

pattern AsBinary :: a -> As 'Binary a
pattern AsBinary x = As x
{-# COMPLETE AsBinary #-}

pattern AsText :: forall enc a. a -> As ('Text enc) a
pattern AsText x = As x
{-# COMPLETE AsText #-}

pattern AsTaggedBinary :: a -> As ('Tagged 'Binary) a
pattern AsTaggedBinary x = As x
{-# COMPLETE AsTaggedBinary #-}

pattern AsTaggedText :: forall enc a. a -> As ('Tagged ('Text enc)) a
pattern AsTaggedText x = As x
{-# COMPLETE AsTaggedText #-}

asBinary :: As 'Binary a -> a
asBinary (AsBinary x) = x
{-# INLINE asBinary #-}

asText :: forall enc a. As ('Text enc) a -> a
asText (AsText x) = x
{-# INLINE asText #-}

asTaggedBinary :: As ('Tagged 'Binary) a -> a
asTaggedBinary (AsTaggedBinary x) = x
{-# INLINE asTaggedBinary #-}

asTaggedText :: As ('Tagged ('Text enc)) a -> a
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
