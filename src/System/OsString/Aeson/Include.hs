-- This template expects CPP definitions for:
--     PLATFORM_NAME = Posix | Windows
--     PLATFORM_STRING = PosixPath | WindowsPath
--     PLATFORM_STRING_SINGLE = 'PosixPath' | 'WindowsPath'
--     PLATFORM_STRING_DOUBLE = "PosixPath" | "WindowsPath"
--     PLATFORM_CHAR = PosixChar | WindowsChar
--     PLATFORM_CHAR_SINGLE = 'PosixChar' | 'WindowsChar'
--     PLATFORM_WORD = Word8 | Word16
--     PLATFORM_WORD_SINGLE = 'Word8' | 'Word16'
--     PLATFORM_UTF_CODEC = UTF8 | UTF16-LE
--     IS_WINDOWS = 0 | 1
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module provides the necessary tools to decode\/encode a
-- PLATFORM_STRING_SINGLE from\/to a 'Value'.
--
-- A PLATFORM_STRING_SINGLE are is a bunch of bytes with no encoding information
-- attached. That means that if you pass such a path for example to 'toJSON' it
-- is not clear which representation we can choose for JSON. If the path was
-- Unicode-encoded we could convert it to a JSON 'Aeson.String' like it is done
-- for 'String', but we cannot assume that for the types found in the
-- @os-string@ package. Hence there are no \"obvious\" 'FromJSON' and 'ToJSON'
-- for PLATFORM_STRING_SINGLE.
--
-- __What this module provides:__
-- This module defines functions and types suitable to convert a
-- PLATFORM_STRING_SINGLE from\/to four JSON representations - Two basic ones
-- and two safe ones. We consider the basic ones first:
--
-- * The /binary/ representation encodes\/decodes a PLATFORM_STRING_SINGLE as a
--   sequence of numbers in JSON, where each number represents the numeric
--   encoding of one PLATFORM_CHAR_SINGLE:
--
--     >>> Data.Aeson.encode (toBinary [pstr|foo/bar|])
--     "[102,111,111,92,98,97,114]"
--
--     Note that this is a total encoding.
--
-- * The /textual/ representation tries to encode\/decode a
--   PLATFORM_STRING_SINGLE as a string in JSON. In order to do that we also
--   have to provide an encoding.
--
--     Some functions in this module take a 'System.IO.TextEncoding' as an
--     argument and you use those defined in "System.IO" or
--     "System.OsString.Encoding":
--
--     >>> Data.Aeson.encode (unsafeToTextWith unicode [pstr|foo/bar|])
--     "\"foo/bar\""
--
--     Other functions expect that the encoding is passed on the type-level
--     (you need the @TypeApplications@ language extensions for this to work):
--
--     >>> Data.Aeson.encode (unsafeToText @Unicode [pstr|foo/bar|])
--     "\"foo/bar\""
--
--     This module provides the encoding types 'Utf8', 'Utf16LE' and 'Unicode',
--     where the latter one of the former two depending on the platform.
--
--     __WARNING:__ Decoding and encoding may fail with a
--     'System.OsString.EncodingException'!
--     The examples above work because 'pstr' encodes to the appropriate Unicode
--     encoding for the particular platform.
--
-- TODO: Write something about the tagged encodings.
module System.OsString.Aeson.PLATFORM_NAME (
    -- * Conversion functions
    -- $funtions
    defaultParseJSON,
    defaultToJSON,
    defaultToEncoding,
    fromBinary,
    fromText,
    fromTextWith,
    fromTagged,
    toBinary,
    toBinaryEncoding,
    toText,
    toTextWith,
    toTextEncoding,
    toTextEncodingWith,
    toTagged,
    toTaggedM,
    toTaggedEncoding,
    toTaggedEncodingM,
    unsafeToText,
    unsafeToTextWith,
    unsafeToTextEncoding,
    unsafeToTextEncodingWith,

    -- * Conversion using newtype wrappers
    -- $newtypes
    As (As, AsBinary, AsText, AsTaggedBinary, AsTaggedText),
    Tag (..),
    Level (..),

    -- * Text encodings
    TextEncoding,
    IsTextEncoding,
    Unicode,
    Utf8,
    Utf16LE,
    unicode,
) where

import Control.Applicative ((<|>))
import Control.Exception (Exception (displayException), displayException)
import Control.Monad (guard, (<=<))
import Control.Monad.Catch (MonadThrow, throwM)
import Data.Aeson.Encoding qualified as Aeson
import Data.Aeson.Types (
    Encoding,
    FromJSON (..),
    FromJSONKey (..),
    Parser,
    ToJSON (..),
    ToJSONKey (..),
    Value,
    (.:),
    (.=),
 )
import Data.Aeson.Types qualified as Aeson
import Data.Coerce (coerce)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Typeable (Typeable)
import Data.Word (PLATFORM_WORD)
import System.IO (TextEncoding)
import System.OsString.Internal.Types (PLATFORM_CHAR (..))
import System.OsString.PLATFORM_NAME (PLATFORM_STRING)
import System.OsString.PLATFORM_NAME qualified as OsString
import Type.Reflection (typeRep)

import System.OsString.Aeson.Types

#if IS_WINDOWS
import System.IO (utf16le)
#else
import System.IO (utf8)
#endif

--------------------------------------------------------------------------------
-- Conversion functions
--------------------------------------------------------------------------------

-- $functions
-- TODO

defaultParseJSON
    :: Value
    -> Parser PLATFORM_STRING
defaultParseJSON value =
    ( coerce @(As ('Tagged ('Text Unicode)) PLATFORM_STRING)
        <$> fromTagged fromText value
    )
        <|> (coerce <$> fromTagged fromBinary value)
{-# INLINE defaultParseJSON #-}

defaultToJSON
    :: PLATFORM_STRING
    -> Value
defaultToJSON x =
    fromMaybe
        (toTagged toBinary (AsTaggedBinary x))
        (toTaggedM toText (AsTaggedText @Unicode x))
{-# INLINE defaultToJSON #-}

defaultToEncoding
    :: PLATFORM_STRING
    -> Encoding
defaultToEncoding x =
    fromMaybe
        (toTaggedEncoding toBinaryEncoding (AsTaggedBinary x))
        (toTaggedEncodingM toTextEncoding (AsTaggedText @Unicode x))
{-# INLINE defaultToEncoding #-}

----------------------------------------
-- Binary
----------------------------------------

fromBinary
    :: Value
    -> Parser
        ( As
            'Binary
            PLATFORM_STRING
        )
fromBinary value =
    As
        . OsString.pack
        . coerce @[PLATFORM_WORD] @[PLATFORM_CHAR]
        <$> parseJSON value
{-# INLINE fromBinary #-}

toBinary
    :: As
        'Binary
        PLATFORM_STRING
    -> Value
toBinary =
    toJSON
        . coerce @[PLATFORM_CHAR] @[PLATFORM_WORD]
        . OsString.unpack
        . unAs
{-# INLINE toBinary #-}

toBinaryEncoding
    :: As
        'Binary
        PLATFORM_STRING
    -> Encoding
toBinaryEncoding =
    toEncoding
        . coerce @[PLATFORM_CHAR] @[PLATFORM_WORD]
        . OsString.unpack
        . unAs
{-# INLINE toBinaryEncoding #-}

----------------------------------------
-- Text
----------------------------------------

fromText
    :: forall enc
     . (IsTextEncoding enc)
    => Value
    -> Parser
        ( As
            ('Text enc)
            PLATFORM_STRING
        )
fromText = fmap As . fromTextWith (textEncoding @enc)
{-# INLINE fromText #-}

fromTextWith
    :: TextEncoding
    -> Value
    -> Parser PLATFORM_STRING
fromTextWith enc = unsafeEncodeWith enc <=< parseJSON
{-# INLINE fromTextWith #-}

toText
    :: forall enc m
     . (IsTextEncoding enc, MonadThrow m)
    => As
        ('Text enc)
        PLATFORM_STRING
    -> m Value
toText = toTextWith (textEncoding @enc) . unAs
{-# INLINE toText #-}

toTextWith
    :: (MonadThrow m)
    => TextEncoding
    -> PLATFORM_STRING
    -> m Value
toTextWith enc =
    either throwM (pure . toJSON) . OsString.decodeWith enc
{-# INLINE toTextWith #-}

unsafeToText
    :: forall enc
     . (IsTextEncoding enc)
    => As
        ('Text enc)
        PLATFORM_STRING
    -> Value
unsafeToText = unsafeToTextWith (textEncoding @enc) . unAs
{-# INLINE unsafeToText #-}

unsafeToTextWith
    :: TextEncoding
    -> PLATFORM_STRING
    -> Value
unsafeToTextWith enc =
    either (error . displayException) id . toTextWith enc
{-# INLINE unsafeToTextWith #-}

toTextEncoding
    :: forall enc m
     . (IsTextEncoding enc, MonadThrow m)
    => As
        ('Text enc)
        PLATFORM_STRING
    -> m Encoding
toTextEncoding = toTextEncodingWith (textEncoding @enc) . unAs
{-# INLINE toTextEncoding #-}

toTextEncodingWith
    :: (MonadThrow m)
    => TextEncoding
    -> PLATFORM_STRING
    -> m Encoding
toTextEncodingWith enc =
    either throwM (pure . toEncoding) . OsString.decodeWith enc
{-# INLINE toTextEncodingWith #-}

unsafeToTextEncoding
    :: forall enc
     . (IsTextEncoding enc)
    => As
        ('Text enc)
        PLATFORM_STRING
    -> Encoding
unsafeToTextEncoding = unsafeToTextEncodingWith (textEncoding @enc) . unAs
{-# INLINE unsafeToTextEncoding #-}

unsafeToTextEncodingWith
    :: TextEncoding
    -> PLATFORM_STRING
    -> Encoding
unsafeToTextEncodingWith enc =
    either (error . displayException) id . toTextEncodingWith enc
{-# INLINE unsafeToTextEncodingWith #-}

----------------------------------------
-- Tagged
----------------------------------------

fromTagged
    :: forall (t :: Tag 'Nested)
     . (Typeable t, Typeable (TagEncoding t))
    => (Value -> Parser (As t PLATFORM_STRING))
    -> Value
    -> Parser
        ( As
            ('Tagged t)
            PLATFORM_STRING
        )
fromTagged decode = Aeson.withObject name $ \obj -> do
    platform <- obj .: "platform"
    guard (platform == (PLATFORM_NAME_DOUBLE :: Text))
    encoding <- obj .: "encoding"
    guard (encoding == show (typeRep @(TagEncoding t)))
    mkTagged <$> (decode =<< (obj .: "payload"))
    where
        name =
            show
                ( typeRep
                    @( As
                        ('Tagged t)
                        PLATFORM_STRING
                     )
                )
{-# INLINE fromTagged #-}

toTagged
    :: forall (t :: Tag 'Nested)
     . (Typeable (TagEncoding t))
    => (As t PLATFORM_STRING -> Value)
    -> As
        ('Tagged t)
        PLATFORM_STRING
    -> Value
toTagged encode x =
    let
        payload = (encode . coerce) x
    in
        Aeson.object
            [ "platform" .= (PLATFORM_NAME_DOUBLE :: Text)
            , "encoding" .= show (typeRep @(TagEncoding t))
            , "payload" .= payload
            ]
{-# INLINE toTagged #-}

toTaggedM
    :: forall (t :: Tag 'Nested) m
     . (MonadThrow m, Typeable (TagEncoding t))
    => (As t PLATFORM_STRING -> m Value)
    -> As
        ('Tagged t)
        PLATFORM_STRING
    -> m Value
toTaggedM encode x = do
    payload <- (encode . coerce) x
    pure . Aeson.object $
        [ "platform" .= (PLATFORM_NAME_DOUBLE :: Text)
        , "encoding" .= show (typeRep @(TagEncoding t))
        , "payload" .= payload
        ]
{-# INLINE toTaggedM #-}

toTaggedEncoding
    :: forall (t :: Tag 'Nested)
     . (Typeable (TagEncoding t))
    => (As t PLATFORM_STRING -> Encoding)
    -> As
        ('Tagged t)
        PLATFORM_STRING
    -> Encoding
toTaggedEncoding encode x =
    let
        payload = (encode . coerce) x
    in
        Aeson.pairs
            ( "platform" .= (PLATFORM_NAME_DOUBLE :: Text)
                <> "encoding" .= show (typeRep @(TagEncoding t))
                <> Aeson.pair "payload" payload
            )
{-# INLINE toTaggedEncoding #-}

toTaggedEncodingM
    :: forall (t :: Tag 'Nested) m
     . (MonadThrow m, Typeable (TagEncoding t))
    => (As t PLATFORM_STRING -> m Encoding)
    -> As
        ('Tagged t)
        PLATFORM_STRING
    -> m Encoding
toTaggedEncodingM encode x = do
    payload <- (encode . coerce) x
    pure . Aeson.pairs $
        "platform" .= (PLATFORM_NAME_DOUBLE :: Text)
            <> "encoding" .= show (typeRep @(TagEncoding t))
            <> Aeson.pair "payload" payload
{-# INLINE toTaggedEncodingM #-}

--------------------------------------------------------------------------------
-- Default instances
--------------------------------------------------------------------------------

-- | This instance tries to decode a PLATFORM_STRING_SINGLE using the
-- PLATFORM_UTF_CODEC encoding. If decoding fails a runtime error will be thrown.
deriving via
    ( As
        ('Text Unicode)
        PLATFORM_STRING
    )
    instance
        FromJSON PLATFORM_STRING

-- | This instance assumes that PLATFORM_STRING_SINGLE is PLATFORM_UTF_CODEC
-- encoded. If encoding fails a runtime error will be thrown.
deriving via
    ( As
        ('Text Unicode)
        PLATFORM_STRING
    )
    instance
        ToJSON PLATFORM_STRING

-- | This instance tries to decode a PLATFORM_STRING_SINGLE using the
-- PLATFORM_UTF_CODEC encoding. If decoding fails a runtime error will be thrown.
deriving via
    ( As
        ('Text Unicode)
        PLATFORM_STRING
    )
    instance
        FromJSONKey PLATFORM_STRING

-- | This instance assumes that PLATFORM_STRING_SINGLE is PLATFORM_UTF_CODEC
-- encoded. If encoding fails a runtime error will be thrown.
deriving via
    ( As
        ('Text Unicode)
        PLATFORM_STRING
    )
    instance
        ToJSONKey PLATFORM_STRING

--------------------------------------------------------------------------------
-- Instances for newtype wrappers
--------------------------------------------------------------------------------

-- $newtypes
-- In addition to the conversion functions this module provides newtype wrappers
-- to control the conversion between JSON value and a PLATFORM_STRING_SINGLE:
--
--  * A path wrapped in a 'AsBinary' uses 'fromBinary' and 'toBinary' in its
--    'FromJSON' instance and 'ToJSON' instance respectively.
--    For example:
--
--    >>> Data.Aeson.encode (AsBinary [pstr|foo/bar|])
--    "[102,111,111,92,98,97,114]"
--
--  * A path wrapped in a 'AsText' uses 'fromText' and 'unsafeToText' in its
--    'FromJSON' instance and 'ToJSON' instance respectively.
--    The encoding used is determined by the type parameter of 'AsText', i.e.
--    a @AsText enc PLATFORM_STRING@ will be encoded to the textual
--    representation using the encoding @enc@.

----------------------------------------
-- Binary
----------------------------------------

instance
    FromJSON
        ( As
            'Binary
            PLATFORM_STRING
        )
    where
    parseJSON = fromBinary
    {-# INLINE parseJSON #-}

instance
    ToJSON
        ( As
            'Binary
            PLATFORM_STRING
        )
    where
    toJSON = toBinary
    {-# INLINE toJSON #-}
    toEncoding = toBinaryEncoding
    {-# INLINE toEncoding #-}

instance
    FromJSONKey
        ( As
            'Binary
            PLATFORM_STRING
        )
    where
    fromJSONKey = Aeson.FromJSONKeyValue parseJSON
    {-# INLINE fromJSONKey #-}

instance
    ToJSONKey
        ( As
            'Binary
            PLATFORM_STRING
        )
    where
    toJSONKey = Aeson.ToJSONKeyValue toJSON toEncoding
    {-# INLINE toJSONKey #-}

----------------------------------------
-- Text
----------------------------------------

instance
    (IsTextEncoding enc)
    => FromJSON
        ( As
            ('Text enc)
            PLATFORM_STRING
        )
    where
    parseJSON = fromText
    {-# INLINE parseJSON #-}

instance
    (IsTextEncoding enc)
    => ToJSON
        ( As
            ('Text enc)
            PLATFORM_STRING
        )
    where
    toJSON = unsafeToText
    {-# INLINE toJSON #-}
    toEncoding = unsafeToTextEncoding
    {-# INLINE toEncoding #-}

instance
    (IsTextEncoding enc)
    => FromJSONKey
        ( As
            ('Text enc)
            PLATFORM_STRING
        )
    where
    fromJSONKey =
        Aeson.FromJSONKeyTextParser
            (fmap As . unsafeEncodeWith (textEncoding @enc) . Text.unpack)
    {-# INLINE fromJSONKey #-}

instance
    (IsTextEncoding enc)
    => ToJSONKey
        ( As
            ('Text enc)
            PLATFORM_STRING
        )
    where
    toJSONKey =
        Aeson.toJSONKeyText
            (Text.pack . unsafeDecodeWith (textEncoding @enc) . unAs)
    {-# INLINE toJSONKey #-}

----------------------------------------
-- Tagged
----------------------------------------

instance
    (FromJSON (As t PLATFORM_STRING), Typeable t, Typeable (TagEncoding t))
    => FromJSON
        ( As
            ('Tagged t)
            PLATFORM_STRING
        )
    where
    parseJSON = fromTagged parseJSON
    {-# INLINE parseJSON #-}

instance
    (ToJSON (As t PLATFORM_STRING), Typeable t, Typeable (TagEncoding t))
    => ToJSON
        ( As
            ('Tagged t)
            PLATFORM_STRING
        )
    where
    toJSON = toTagged toJSON
    {-# INLINE toJSON #-}
    toEncoding = toTaggedEncoding toEncoding
    {-# INLINE toEncoding #-}

instance
    (FromJSON (As t PLATFORM_STRING), Typeable t, Typeable (TagEncoding t))
    => FromJSONKey
        ( As
            ('Tagged t)
            PLATFORM_STRING
        )
    where
    fromJSONKey = Aeson.FromJSONKeyValue parseJSON
    {-# INLINE fromJSONKey #-}

instance
    (ToJSON (As t PLATFORM_STRING), Typeable t, Typeable (TagEncoding t))
    => ToJSONKey
        ( As
            ('Tagged t)
            PLATFORM_STRING
        )
    where
    toJSONKey = Aeson.ToJSONKeyValue toJSON toEncoding
    {-# INLINE toJSONKey #-}

--------------------------------------------------------------------------------
-- Other types and functions
--------------------------------------------------------------------------------

#if IS_WINDOWS
type Unicode = Utf16LE
#else
type Unicode = Utf8
#endif

unicode :: TextEncoding
#if IS_WINDOWS
unicode = utf16le
#else
unicode = utf8
#endif
{-# INLINE unicode #-}

--------------------------------------------------------------------------------
-- Internal helpers
--------------------------------------------------------------------------------

mkTagged
    :: As t PLATFORM_STRING
    -> As
        ('Tagged t)
        PLATFORM_STRING
mkTagged = coerce
{-# INLINE mkTagged #-}

unsafeDecodeWith
    :: TextEncoding
    -> PLATFORM_STRING
    -> String
unsafeDecodeWith enc =
    either (error . displayException) id . OsString.decodeWith enc
{-# INLINE unsafeDecodeWith #-}

unsafeEncodeWith
    :: TextEncoding
    -> String
    -> Parser PLATFORM_STRING
unsafeEncodeWith enc =
    either (fail . displayException) pure . OsString.encodeWith enc
{-# INLINE unsafeEncodeWith #-}
