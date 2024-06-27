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
--     >>> Data.Aeson.encode (toTextWith unicode [pstr|foo/bar|])
--     "\"foo/bar\""
--
--     Other functions expect that the encoding is passed on the type-level
--     (you need the @TypeApplications@ language extensions for this to work):
--
--     >>> Data.Aeson.encode (toText @Unicode [pstr|foo/bar|])
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
    toTaggedEncoding,

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

import Control.Exception (displayException)
import Control.Monad (guard, (<=<))
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
import Data.Text (Text)
import Data.Text qualified as Text
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

----------------------------------------
-- Binary
----------------------------------------

fromBinary :: Value -> Parser PLATFORM_STRING
fromBinary value =
    OsString.pack . coerce @[PLATFORM_WORD] @[PLATFORM_CHAR] <$> parseJSON value
{-# INLINE fromBinary #-}

toBinary :: PLATFORM_STRING -> Value
toBinary =
    toJSON
        . coerce @[PLATFORM_CHAR] @[PLATFORM_WORD]
        . OsString.unpack
{-# INLINE toBinary #-}

toBinaryEncoding :: PLATFORM_STRING -> Encoding
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
    :: forall enc
     . (IsTextEncoding enc)
    => PLATFORM_STRING
    -> Value
toText = toTextWith (textEncoding @enc)
{-# INLINE toText #-}

toTextWith :: TextEncoding -> PLATFORM_STRING -> Value
toTextWith enc = toJSON . unsafeDecodeWith enc
{-# INLINE toTextWith #-}

toTextEncoding
    :: forall enc
     . (IsTextEncoding enc)
    => PLATFORM_STRING
    -> Encoding
toTextEncoding = toTextEncodingWith (textEncoding @enc)
{-# INLINE toTextEncoding #-}

toTextEncodingWith :: TextEncoding -> PLATFORM_STRING -> Encoding
toTextEncodingWith enc = toEncoding . unsafeDecodeWith enc
{-# INLINE toTextEncodingWith #-}

----------------------------------------
-- Tagged
----------------------------------------

fromTagged
    :: (Value -> Parser PLATFORM_STRING)
    -> Value
    -> Parser PLATFORM_STRING
fromTagged = fromTagged'
{-# INLINE fromTagged #-}

toTagged :: (PLATFORM_STRING -> Value) -> PLATFORM_STRING -> Value
toTagged encode path = taggedAs (encode path)
{-# INLINE toTagged #-}

toTaggedEncoding :: (PLATFORM_STRING -> Encoding) -> PLATFORM_STRING -> Encoding
toTaggedEncoding encode path = taggedAsEncoding (encode path)
{-# INLINE toTaggedEncoding #-}

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
--  * A path wrapped in a 'AsText' uses 'fromText' and 'toText' in its
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
    toJSON = toText @enc . asText
    {-# INLINE toJSON #-}
    toEncoding = toTextEncoding @enc . asText
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

instance (FromJSON a) => FromJSON (Tagged a) where
    parseJSON value = Tagged <$> fromTagged' parseJSON value
    {-# INLINE parseJSON #-}

instance (ToJSON a) => ToJSON (Tagged a) where
    toJSON = taggedAs . toJSON . tagged
    {-# INLINE toJSON #-}
    toEncoding = taggedAsEncoding . toEncoding . tagged
    {-# INLINE toEncoding #-}

instance (FromJSON a) => FromJSONKey (Tagged a) where
    fromJSONKey = Aeson.FromJSONKeyValue parseJSON
    {-# INLINE fromJSONKey #-}

instance (ToJSON a) => ToJSONKey (Tagged a) where
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
    :: (Value -> Parser a)
    -> Value
    -> Parser a
fromTagged' decode = Aeson.withObject name $
    \obj -> do
        tag <- obj .: "tag"
        guard (tag == (PLATFORM_STRING_DOUBLE :: Text))
        decode =<< (obj .: "payload")
    where
        name = "Tagged " <>
            PLATFORM_STRING_DOUBLE
{-# INLINE fromTagged' #-}

taggedAs :: Value -> Value
taggedAs value =
    Aeson.object
        [ "tag" .= show (typeRep @PLATFORM_STRING)
        , "payload" .= value
        ]
{-# INLINE taggedAs #-}

taggedAsEncoding :: Encoding -> Encoding
taggedAsEncoding payload =
    Aeson.pairs
        ( "tag" .= show (typeRep @PLATFORM_STRING)
            <> Aeson.pair "payload" payload
        )
{-# INLINE taggedAsEncoding #-}

unsafeDecodeWith :: TextEncoding -> PLATFORM_STRING -> String
unsafeDecodeWith enc =
    either (error . displayException) id . OsString.decodeWith enc
{-# INLINE unsafeDecodeWith #-}

unsafeEncodeWith :: TextEncoding -> String -> Parser PLATFORM_STRING
unsafeEncodeWith enc =
    either (fail . displayException) pure . OsString.encodeWith enc
{-# INLINE unsafeEncodeWith #-}
