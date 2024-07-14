{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module System.OsString.Aeson.Types where

import Data.Aeson (Value (..))
import Data.Kind (Type)
import Data.Text qualified as Text
import Data.Typeable (Typeable)
import System.IO (TextEncoding, utf16le, utf8)
import Type.Reflection (typeRep)

newtype As (t :: k) a = As {unAs :: a}
    deriving (Eq, Foldable, Functor, Show, Traversable)

data Level
    = Nested
    | TopLevel

data Tag :: Level -> Type where
    Binary :: Tag a
    Text :: (enc :: Type) -> Tag a
    Tagged :: Tag 'Nested -> Tag 'TopLevel

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
    :: forall {l :: Level} (enc :: Type) a. a -> As ('Text enc :: Tag l) a
#else
pattern AsText
    :: forall (l :: Level) (enc :: Type) a. a -> As ('Text enc :: Tag l) a
#endif
pattern AsText x = As x

{-# COMPLETE AsText #-}

pattern AsTaggedBinary
    :: forall a. a -> As ('Tagged 'Binary :: Tag 'TopLevel) a
pattern AsTaggedBinary x = As x

{-# COMPLETE AsTaggedBinary #-}

pattern AsTaggedText
    :: forall (enc :: Type) a. a -> As ('Tagged ('Text enc) :: Tag 'TopLevel) a
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

class TagEncoding (t :: Tag l) where
    tagEncoding :: Value

instance TagEncoding 'Binary where
    tagEncoding = Null

instance (Typeable enc) => TagEncoding ('Text enc) where
    tagEncoding = (String . Text.pack . show) (typeRep @enc)

instance (TagEncoding t) => TagEncoding ('Tagged t) where
    tagEncoding = tagEncoding @_ @t

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
