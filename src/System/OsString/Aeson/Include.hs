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
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
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
    AsBinary (..),
    AsText (..),
    Tagged (..),

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
    fromTagged @Unicode (fromText @Unicode) value
        <|> fromTagged @() fromBinary value
{-# INLINE defaultParseJSON #-}

defaultToJSON
    :: PLATFORM_STRING
    -> Value
defaultToJSON x =
    fromMaybe
        (toTagged @() toBinary x)
        (toTaggedM @Unicode (toText @Unicode) x)
{-# INLINE defaultToJSON #-}

defaultToEncoding
    :: PLATFORM_STRING
    -> Encoding
defaultToEncoding x =
    fromMaybe
        (toTaggedEncoding @() toBinaryEncoding x)
        (toTaggedEncodingM @Unicode (toTextEncoding @Unicode) x)
{-# INLINE defaultToEncoding #-}

----------------------------------------
-- Binary
----------------------------------------

fromBinary
    :: Value
    -> Parser PLATFORM_STRING
fromBinary value =
    OsString.pack
        . coerce @[PLATFORM_WORD] @[PLATFORM_CHAR]
        <$> parseJSON value
{-# INLINE fromBinary #-}

toBinary
    :: PLATFORM_STRING
    -> Value
toBinary =
    toJSON
        . coerce @[PLATFORM_CHAR] @[PLATFORM_WORD]
        . OsString.unpack
{-# INLINE toBinary #-}

toBinaryEncoding
    :: PLATFORM_STRING
    -> Encoding
toBinaryEncoding =
    toEncoding
        . coerce @[PLATFORM_CHAR] @[PLATFORM_WORD]
        . OsString.unpack
{-# INLINE toBinaryEncoding #-}

----------------------------------------
-- Text
----------------------------------------

fromText
    :: forall enc
     . (IsTextEncoding enc)
    => Value
    -> Parser PLATFORM_STRING
fromText = fromTextWith (textEncoding @enc)
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
    => PLATFORM_STRING
    -> m Value
toText = toTextWith (textEncoding @enc)
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
    => PLATFORM_STRING
    -> Value
unsafeToText = unsafeToTextWith (textEncoding @enc)
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
    => PLATFORM_STRING
    -> m Encoding
toTextEncoding = toTextEncodingWith (textEncoding @enc)
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
    => PLATFORM_STRING
    -> Encoding
unsafeToTextEncoding = unsafeToTextEncodingWith (textEncoding @enc)
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
    :: forall enc
     . (Typeable enc)
    => (Value -> Parser PLATFORM_STRING)
    -> Value
    -> Parser PLATFORM_STRING
fromTagged = fromTagged' @enc
{-# INLINE fromTagged #-}

toTagged
    :: forall enc
     . (Typeable enc)
    => (PLATFORM_STRING -> Value)
    -> PLATFORM_STRING
    -> Value
toTagged encode path = taggedAs' @enc (encode path)
{-# INLINE toTagged #-}

toTaggedM
    :: forall enc m
     . (MonadThrow m, Typeable enc)
    => (PLATFORM_STRING -> m Value)
    -> PLATFORM_STRING
    -> m Value
toTaggedM encode path = taggedAs' @enc <$> encode path
{-# INLINE toTaggedM #-}

toTaggedEncoding
    :: forall enc
     . (Typeable enc)
    => (PLATFORM_STRING -> Encoding)
    -> PLATFORM_STRING
    -> Encoding
toTaggedEncoding encode path = taggedAsEncoding @enc (encode path)
{-# INLINE toTaggedEncoding #-}

toTaggedEncodingM
    :: forall enc m
     . (MonadThrow m, Typeable enc)
    => (PLATFORM_STRING -> m Encoding)
    -> PLATFORM_STRING
    -> m Encoding
toTaggedEncodingM encode path = taggedAsEncoding @enc <$> encode path
{-# INLINE toTaggedEncodingM #-}

--------------------------------------------------------------------------------
-- Default instances
--------------------------------------------------------------------------------

-- | This instance tries to decode a PLATFORM_STRING_SINGLE using the
-- PLATFORM_UTF_CODEC encoding. If decoding fails a runtime error will be thrown.
deriving via (AsText Unicode PLATFORM_STRING) instance FromJSON PLATFORM_STRING

-- | This instance assumes that PLATFORM_STRING_SINGLE is PLATFORM_UTF_CODEC
-- encoded. If encoding fails a runtime error will be thrown.
deriving via (AsText Unicode PLATFORM_STRING) instance ToJSON PLATFORM_STRING

-- | This instance tries to decode a PLATFORM_STRING_SINGLE using the
-- PLATFORM_UTF_CODEC encoding. If decoding fails a runtime error will be thrown.
deriving via (AsText Unicode PLATFORM_STRING) instance FromJSONKey PLATFORM_STRING

-- | This instance assumes that PLATFORM_STRING_SINGLE is PLATFORM_UTF_CODEC
-- encoded. If encoding fails a runtime error will be thrown.
deriving via (AsText Unicode PLATFORM_STRING) instance ToJSONKey PLATFORM_STRING

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

instance FromJSON (AsBinary PLATFORM_STRING) where
    parseJSON value = AsBinary <$> fromBinary value
    {-# INLINE parseJSON #-}

instance ToJSON (AsBinary PLATFORM_STRING) where
    toJSON = toBinary . asBinary
    {-# INLINE toJSON #-}
    toEncoding = toBinaryEncoding . asBinary
    {-# INLINE toEncoding #-}

instance FromJSONKey (AsBinary PLATFORM_STRING) where
    fromJSONKey = Aeson.FromJSONKeyValue parseJSON
    {-# INLINE fromJSONKey #-}

instance ToJSONKey (AsBinary PLATFORM_STRING) where
    toJSONKey = Aeson.ToJSONKeyValue toJSON toEncoding
    {-# INLINE toJSONKey #-}

----------------------------------------
-- Text
----------------------------------------

instance (IsTextEncoding enc) => FromJSON (AsText enc PLATFORM_STRING) where
    parseJSON value = AsText <$> fromText @enc value
    {-# INLINE parseJSON #-}

instance (IsTextEncoding enc) => ToJSON (AsText enc PLATFORM_STRING) where
    toJSON = unsafeToText @enc . asText
    {-# INLINE toJSON #-}
    toEncoding = unsafeToTextEncoding @enc . asText
    {-# INLINE toEncoding #-}

instance (IsTextEncoding enc) => FromJSONKey (AsText enc PLATFORM_STRING) where
    fromJSONKey =
        Aeson.FromJSONKeyTextParser
            (fmap AsText . unsafeEncodeWith (textEncoding @enc) . Text.unpack)
    {-# INLINE fromJSONKey #-}

instance (IsTextEncoding enc) => ToJSONKey (AsText enc PLATFORM_STRING) where
    toJSONKey =
        Aeson.toJSONKeyText
            (Text.pack . unsafeDecodeWith (textEncoding @enc) . asText)
    {-# INLINE toJSONKey #-}

----------------------------------------
-- Tagged
----------------------------------------

instance (FromJSON (AsBinary a)) => FromJSON (Tagged (AsBinary a)) where
    parseJSON value = Tagged <$> fromTagged' @() parseJSON value
    {-# INLINE parseJSON #-}

instance (IsTextEncoding enc, FromJSON (AsText enc a)) => FromJSON (Tagged (AsText enc a)) where
    parseJSON value = Tagged <$> fromTagged' @enc parseJSON value
    {-# INLINE parseJSON #-}

instance (ToJSON (AsBinary a)) => ToJSON (Tagged (AsBinary a)) where
    toJSON = taggedAs . tagged
    {-# INLINE toJSON #-}
    toEncoding = taggedAsEncoding @() . toEncoding . tagged
    {-# INLINE toEncoding #-}

instance (IsTextEncoding enc, ToJSON (AsText enc a)) => ToJSON (Tagged (AsText enc a)) where
    toJSON = taggedAs . tagged
    {-# INLINE toJSON #-}
    toEncoding = taggedAsEncoding @enc . toEncoding . tagged
    {-# INLINE toEncoding #-}

instance (FromJSON (AsBinary a)) => FromJSONKey (Tagged (AsBinary a)) where
    fromJSONKey = Aeson.FromJSONKeyValue parseJSON
    {-# INLINE fromJSONKey #-}

instance (IsTextEncoding enc, FromJSON (AsText enc a)) => FromJSONKey (Tagged (AsText enc a)) where
    fromJSONKey = Aeson.FromJSONKeyValue parseJSON
    {-# INLINE fromJSONKey #-}

instance (ToJSON (AsBinary a)) => ToJSONKey (Tagged (AsBinary a)) where
    toJSONKey = Aeson.ToJSONKeyValue toJSON toEncoding
    {-# INLINE toJSONKey #-}

instance (IsTextEncoding enc, ToJSON (AsText enc a)) => ToJSONKey (Tagged (AsText enc a)) where
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

fromTagged'
    :: forall enc a
     . (Typeable enc)
    => (Value -> Parser a)
    -> Value
    -> Parser a
fromTagged' decode = Aeson.withObject name $
    \obj -> do
        platform <- obj .: "platform"
        guard (platform == (PLATFORM_NAME_DOUBLE :: Text))
        encoding <- obj .: "encoding"
        guard (encoding == show (typeRep @enc))
        decode =<< (obj .: "payload")
    where
        name =
            "Tagged "
                <> PLATFORM_STRING_DOUBLE
                <> " "
                <> show (typeRep @enc)
{-# INLINE fromTagged' #-}

taggedAs :: forall a. (ToJSON a, Typeable (EncodingTag a)) => a -> Value
taggedAs x =
    Aeson.object
        [ "platform" .= (PLATFORM_NAME_DOUBLE :: Text)
        , "encoding" .= show (typeRep @(EncodingTag a))
        , "payload" .= x
        ]
{-# INLINE taggedAs #-}

taggedAs' :: forall enc. (Typeable enc) => Value -> Value
taggedAs' value =
    Aeson.object
        [ "platform" .= (PLATFORM_NAME_DOUBLE :: Text)
        , "encoding" .= show (typeRep @enc)
        , "payload" .= value
        ]
{-# INLINE taggedAs' #-}

taggedAsEncoding :: forall enc. (Typeable enc) => Encoding -> Encoding
taggedAsEncoding payload =
    Aeson.pairs
        ( "platform" .= (PLATFORM_NAME_DOUBLE :: Text)
            <> "encoding" .= show (typeRep @enc)
            <> Aeson.pair "payload" payload
        )
{-# INLINE taggedAsEncoding #-}

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
