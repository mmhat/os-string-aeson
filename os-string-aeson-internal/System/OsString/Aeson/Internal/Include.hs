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
--     BASE64_EXAMPLE = "..."
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module System.OsString.Aeson.Internal.PLATFORM_NAME where

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
import Data.Aeson.Types qualified
import Data.Base64.Types qualified as Base64
import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Short.Base64 qualified as Base64
import Data.Coerce (coerce)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Short qualified as Text.Short
import Data.Typeable (Typeable)
import Data.Word (PLATFORM_WORD)
import System.IO (TextEncoding)
import System.OsString.Aeson.Internal.Types
import System.OsString.Internal.Types (PLATFORM_CHAR (..), PLATFORM_STRING (..))
import System.OsString.PLATFORM_NAME qualified as OsString
import Type.Reflection (typeRep)

#if IS_WINDOWS
import System.IO (utf16le)
#else
import System.IO (utf8)
#endif

-- $setup
-- >>> :set -XOverloadedLists
-- >>> :set -XOverloadedStrings
-- >>> :set -XQuasiQuotes
-- >>> :set -XTypeApplications
-- >>> import Data.Aeson.Types (Value (..))
-- >>> import System.OsString.PLATFORM_NAME (pstr)

--------------------------------------------------------------------------------
-- Conversion functions
--------------------------------------------------------------------------------

-- $functions
-- TODO

defaultParseJSON :: Value -> Parser PLATFORM_STRING
defaultParseJSON value =
    fromTagged (fromTextualAs @Unicode) value
        <|> fromTagged fromBinaryAs value
{-# INLINE defaultParseJSON #-}

defaultToJSON :: PLATFORM_STRING -> Value
defaultToJSON x =
    fromMaybe
        (toTagged toBinaryAs x)
        (toTaggedM (toTextualAs @Unicode) x)
{-# INLINE defaultToJSON #-}

defaultToEncoding :: PLATFORM_STRING -> Encoding
defaultToEncoding x =
    fromMaybe
        (toTaggedEncoding toBinaryEncodingAs x)
        (toTaggedEncodingM (toTextualEncodingAs @Unicode) x)
{-# INLINE defaultToEncoding #-}

----------------------------------------
-- Base64
----------------------------------------

-- | Try to parse a PLATFORM_STRING_DOUBLE from the base64-text representation:
--
--     >>> Data.Aeson.Types.parseMaybe fromBase64 (String BASE64_EXAMPLE)
--     Just "foo"
fromBase64 :: Value -> Parser PLATFORM_STRING
fromBase64 value =
    either (fail . Text.unpack) (pure . coerce)
        . Base64.decodeBase64Untyped
        . Text.Short.toShortByteString
        =<< parseJSON value
{-# INLINE fromBase64 #-}

-- | A version of 'fromBase64' that returns the resulting PLATFORM_STRING_DOUBLE
-- wrapped in an 'As' newtype.
fromBase64As :: Value -> Parser (As Base64 PLATFORM_STRING)
fromBase64As = fmap As . fromBase64
{-# INLINE fromBase64As #-}

-- | Encode a PLATFORM_STRING_DOUBLE in the base64-text representation:
--
--     >>> toBase64 [pstr|foo|]
--     String BASE64_EXAMPLE
toBase64 :: PLATFORM_STRING -> Value
toBase64 =
    toJSON
        . Base64.extractBase64
        . Base64.encodeBase64
        . coerce @_ @ShortByteString
{-# INLINE toBase64 #-}

-- | A version of 'toBase64' that expects the PLATFORM_STRING_DOUBLE to be
-- wrapped in an 'As' newtype.
toBase64As :: As Base64 PLATFORM_STRING -> Value
toBase64As = toBase64 . unAs
{-# INLINE toBase64As #-}

-- | Similar to 'toBase64', but returns an 'Encoding' instead of a 'Value'.
toBase64Encoding :: PLATFORM_STRING -> Encoding
toBase64Encoding =
    toEncoding
        . Base64.extractBase64
        . Base64.encodeBase64
        . coerce @_ @ShortByteString
{-# INLINE toBase64Encoding #-}

-- | A version of 'toBase64Encoding' that expects the PLATFORM_STRING_DOUBLE to
-- be wrapped in an 'As' newtype.
toBase64EncodingAs :: As Base64 PLATFORM_STRING -> Encoding
toBase64EncodingAs = toBase64Encoding . unAs
{-# INLINE toBase64EncodingAs #-}

----------------------------------------
-- Binary
----------------------------------------

-- | Try to parse a PLATFORM_STRING_DOUBLE from the binary representation:
--
--     >>> Data.Aeson.Types.parseMaybe fromBinary (Array [Number 102,Number 111,Number 111])
--     Just "foo"
fromBinary :: Value -> Parser PLATFORM_STRING
fromBinary value =
    OsString.pack
        . coerce @[PLATFORM_WORD] @[PLATFORM_CHAR]
        <$> parseJSON value
{-# INLINE fromBinary #-}

-- | A version of 'fromBinary' that returns the resulting PLATFORM_STRING_DOUBLE
-- wrapped in an 'As' newtype.
fromBinaryAs :: Value -> Parser (As Binary PLATFORM_STRING)
fromBinaryAs = fmap As . fromBinary
{-# INLINE fromBinaryAs #-}

-- | Encode a PLATFORM_STRING_DOUBLE in the binary representation:
--
--     >>> toBinary [pstr|foo|]
--     Array [Number 102.0,Number 111.0,Number 111.0]
toBinary :: PLATFORM_STRING -> Value
toBinary =
    toJSON
        . coerce @[PLATFORM_CHAR] @[PLATFORM_WORD]
        . OsString.unpack
{-# INLINE toBinary #-}

-- | A version of 'toBinary' that expects the PLATFORM_STRING_DOUBLE to be
-- wrapped in an 'As' newtype.
toBinaryAs :: As Binary PLATFORM_STRING -> Value
toBinaryAs = toBinary . unAs
{-# INLINE toBinaryAs #-}

-- | Similar to 'toBinary', but returns an 'Encoding' instead of a 'Value'.
toBinaryEncoding :: PLATFORM_STRING -> Encoding
toBinaryEncoding =
    toEncoding
        . coerce @[PLATFORM_CHAR] @[PLATFORM_WORD]
        . OsString.unpack
{-# INLINE toBinaryEncoding #-}

-- | A version of 'toBinaryEncoding' that expects the PLATFORM_STRING_DOUBLE to
-- be wrapped in an 'As' newtype.
toBinaryEncodingAs :: As Binary PLATFORM_STRING -> Encoding
toBinaryEncodingAs = toBinaryEncoding . unAs
{-# INLINE toBinaryEncodingAs #-}

----------------------------------------
-- Textual
----------------------------------------

-- | Try to parse a PLATFORM_STRING_DOUBLE from the textual representation:
--
--     >>> Data.Aeson.Types.parseMaybe (fromText @Unicode) "foo"
--     Just "foo"
fromTextual
    :: forall enc
     . (IsTextEncoding enc)
    => Value
    -> Parser PLATFORM_STRING
fromTextual = fromTextualWith (textEncoding @enc)
{-# INLINE fromTextual #-}

-- | A version of 'fromTextual' that returns the resulting
-- PLATFORM_STRING_DOUBLE wrapped in an 'As' newtype.
fromTextualAs
    :: forall enc
     . (IsTextEncoding enc)
    => Value -> Parser (As (Textual enc) PLATFORM_STRING)
fromTextualAs = fmap As . fromTextual @enc
{-# INLINE fromTextualAs #-}

-- | A version of 'fromTextual' that takes the 'TextEncoding' as the first
-- argument.
fromTextualWith :: TextEncoding -> Value -> Parser PLATFORM_STRING
fromTextualWith enc = unsafeEncodeWith enc <=< parseJSON
{-# INLINE fromTextualWith #-}

-- | Encode a PLATFORM_STRING_DOUBLE in the textual representation:
--
--     >>> toText @Unicode [pstr|foo|]
--     String "foo"
toTextual
    :: forall enc m
     . (IsTextEncoding enc, MonadThrow m)
    => PLATFORM_STRING -> m Value
toTextual = toTextualWith (textEncoding @enc)
{-# INLINE toTextual #-}

-- | A version of 'toTextual' that expects the PLATFORM_STRING_DOUBLE to be
-- wrapped in an 'As' newtype.
toTextualAs
    :: forall enc m
     . (IsTextEncoding enc, MonadThrow m)
    => As (Textual enc) PLATFORM_STRING -> m Value
toTextualAs = toTextual @enc . unAs
{-# INLINE toTextualAs #-}

-- | A version of 'toTextual' that takes the 'TextEncoding' as the first
-- argument.
toTextualWith :: (MonadThrow m) => TextEncoding -> PLATFORM_STRING -> m Value
toTextualWith enc =
    either throwM (pure . toJSON) . OsString.decodeWith enc
{-# INLINE toTextualWith #-}

-- | A version of 'toTextual' that is pure, but throws a 'EncodingException' at
-- runtime if it fails to encode the PLATFORM_STRING_DOUBLE.
unsafeToTextual :: forall enc. (IsTextEncoding enc) => PLATFORM_STRING -> Value
unsafeToTextual = unsafeToTextualWith (textEncoding @enc)
{-# INLINE unsafeToTextual #-}

-- | A version of 'toTextualAs' that is pure, but throws a 'EncodingException'
-- at runtime if it fails to encode the PLATFORM_STRING_DOUBLE.
unsafeToTextualAs
    :: forall enc
     . (IsTextEncoding enc)
    => As (Textual enc) PLATFORM_STRING -> Value
unsafeToTextualAs = unsafeToTextual @enc . unAs
{-# INLINE unsafeToTextualAs #-}

-- | A version of 'toTextualWith' that is pure, but throws a 'EncodingException'
-- at runtime if it fails to encode the PLATFORM_STRING_DOUBLE.
unsafeToTextualWith :: TextEncoding -> PLATFORM_STRING -> Value
unsafeToTextualWith enc =
    either (error . displayException) id . toTextualWith enc
{-# INLINE unsafeToTextualWith #-}

-- | Similar to 'toTextual', but returns an 'Encoding' instead of a 'Value'.
toTextualEncoding
    :: forall enc m
     . (IsTextEncoding enc, MonadThrow m)
    => PLATFORM_STRING -> m Encoding
toTextualEncoding = toTextualEncodingWith (textEncoding @enc)
{-# INLINE toTextualEncoding #-}

-- | A version of 'toTextualEncoding' that expects the PLATFORM_STRING_DOUBLE to
-- be wrapped in an 'As' newtype.
toTextualEncodingAs
    :: forall enc m
     . (IsTextEncoding enc, MonadThrow m)
    => As (Textual enc) PLATFORM_STRING -> m Encoding
toTextualEncodingAs = toTextualEncoding @enc . unAs
{-# INLINE toTextualEncodingAs #-}

-- | A version of 'toTextualEncoding' that takes the 'TextEncoding' as the first
-- argument.
toTextualEncodingWith
    :: (MonadThrow m)
    => TextEncoding -> PLATFORM_STRING -> m Encoding
toTextualEncodingWith enc =
    either throwM (pure . toEncoding) . OsString.decodeWith enc
{-# INLINE toTextualEncodingWith #-}

-- | A version of 'toTextualEncoding' that is pure, but throws a
-- 'EncodingException' at runtime if it fails to encode the
-- PLATFORM_STRING_DOUBLE.
unsafeToTextualEncoding
    :: forall enc
     . (IsTextEncoding enc)
    => PLATFORM_STRING -> Encoding
unsafeToTextualEncoding = unsafeToTextualEncodingWith (textEncoding @enc)
{-# INLINE unsafeToTextualEncoding #-}

-- | A version of 'toTextualEncodingAs' that is pure, but throws a
-- 'EncodingException' at runtime if it fails to encode the
-- PLATFORM_STRING_DOUBLE.
unsafeToTextualEncodingAs
    :: forall enc
     . (IsTextEncoding enc)
    => As (Textual enc) PLATFORM_STRING -> Encoding
unsafeToTextualEncodingAs = unsafeToTextualEncoding @enc . unAs
{-# INLINE unsafeToTextualEncodingAs #-}

-- | A version of 'toTextualEncodingWith' that is pure, but throws a
-- 'EncodingException' at runtime if it fails to encode the
-- PLATFORM_STRING_DOUBLE.
unsafeToTextualEncodingWith :: TextEncoding -> PLATFORM_STRING -> Encoding
unsafeToTextualEncodingWith enc =
    either (error . displayException) id . toTextualEncodingWith enc
{-# INLINE unsafeToTextualEncodingWith #-}

----------------------------------------
-- Tagged
----------------------------------------

fromTagged
    :: forall t
     . (Typeable t)
    => (Value -> Parser (As t PLATFORM_STRING))
    -> Value
    -> Parser PLATFORM_STRING
fromTagged decode = Data.Aeson.Types.withObject name $ \obj -> do
    platform <- obj .: "platform"
    guard (platform == (PLATFORM_NAME_DOUBLE :: Text))
    unAs <$> (decode =<< (obj .: "data"))
    where
        name = show (typeRep @(As (Tagged t) PLATFORM_STRING))
{-# INLINE fromTagged #-}

fromTaggedAs
    :: (Typeable t)
    => (Value -> Parser (As t PLATFORM_STRING))
    -> Value
    -> Parser (As (Tagged t) PLATFORM_STRING)
fromTaggedAs decode = fmap As . fromTagged decode
{-# INLINE fromTaggedAs #-}

toTagged
    :: (As t PLATFORM_STRING -> Value)
    -> PLATFORM_STRING
    -> Value
toTagged encode x =
    let
        data_ = (encode . coerce) x
    in
        Data.Aeson.Types.object
            [ "platform" .= (PLATFORM_NAME_DOUBLE :: Text)
            , "data" .= data_
            ]
{-# INLINE toTagged #-}

toTaggedAs
    :: (As t PLATFORM_STRING -> Value)
    -> As (Tagged t) PLATFORM_STRING
    -> Value
toTaggedAs encode = toTagged encode . unAs
{-# INLINE toTaggedAs #-}

toTaggedM
    :: (MonadThrow m)
    => (As t PLATFORM_STRING -> m Value)
    -> PLATFORM_STRING
    -> m Value
toTaggedM encode x = do
    data_ <- (encode . coerce) x
    pure . Data.Aeson.Types.object $
        [ "platform" .= (PLATFORM_NAME_DOUBLE :: Text)
        , "data" .= data_
        ]
{-# INLINE toTaggedM #-}

toTaggedAsM
    :: (MonadThrow m)
    => (As t PLATFORM_STRING -> m Value)
    -> As (Tagged t) PLATFORM_STRING
    -> m Value
toTaggedAsM encode = toTaggedM encode . unAs
{-# INLINE toTaggedAsM #-}

toTaggedEncoding
    :: (As t PLATFORM_STRING -> Encoding)
    -> PLATFORM_STRING
    -> Encoding
toTaggedEncoding encode x =
    let
        data_ = (encode . coerce) x
    in
        Aeson.pairs
            ( "platform" .= (PLATFORM_NAME_DOUBLE :: Text)
                <> Aeson.pair "data" data_
            )
{-# INLINE toTaggedEncoding #-}

toTaggedEncodingAs
    :: (As t PLATFORM_STRING -> Encoding)
    -> As (Tagged t) PLATFORM_STRING
    -> Encoding
toTaggedEncodingAs encode = toTaggedEncoding encode . unAs
{-# INLINE toTaggedEncodingAs #-}

toTaggedEncodingM
    :: (MonadThrow m)
    => (As t PLATFORM_STRING -> m Encoding) -> PLATFORM_STRING -> m Encoding
toTaggedEncodingM encode x = do
    data_ <- (encode . coerce) x
    pure . Aeson.pairs $
        "platform" .= (PLATFORM_NAME_DOUBLE :: Text)
            <> Aeson.pair "data" data_
{-# INLINE toTaggedEncodingM #-}

toTaggedEncodingAsM
    :: forall t m
     . (MonadThrow m)
    => (As t PLATFORM_STRING -> m Encoding)
    -> As (Tagged t) PLATFORM_STRING
    -> m Encoding
toTaggedEncodingAsM encode = toTaggedEncodingM encode . unAs
{-# INLINE toTaggedEncodingAsM #-}

--------------------------------------------------------------------------------
-- Default instances
--------------------------------------------------------------------------------

-- | This instance tries to decode a PLATFORM_STRING_SINGLE using the
-- PLATFORM_UTF_CODEC encoding. If decoding fails a runtime error will be
-- thrown.
deriving via
    (As (Textual Unicode) PLATFORM_STRING)
    instance
        FromJSON PLATFORM_STRING

-- | This instance assumes that PLATFORM_STRING_SINGLE is PLATFORM_UTF_CODEC
-- encoded. If encoding fails a runtime error will be thrown.
deriving via
    (As (Textual Unicode) PLATFORM_STRING)
    instance
        ToJSON PLATFORM_STRING

-- | This instance tries to decode a PLATFORM_STRING_SINGLE using the
-- PLATFORM_UTF_CODEC encoding. If decoding fails a runtime error will be
-- thrown.
deriving via
    (As (Textual Unicode) PLATFORM_STRING)
    instance
        FromJSONKey PLATFORM_STRING

-- | This instance assumes that PLATFORM_STRING_SINGLE is PLATFORM_UTF_CODEC
-- encoded. If encoding fails a runtime error will be thrown.
deriving via
    (As (Textual Unicode) PLATFORM_STRING)
    instance
        ToJSONKey PLATFORM_STRING

--------------------------------------------------------------------------------
-- Instances for newtype wrappers
--------------------------------------------------------------------------------

-- $newtypes
-- In addition to the conversion functions this module provides newtype wrappers
-- to control the conversion between JSON value and a PLATFORM_STRING_SINGLE:
--
--  * A path wrapped in a 'AsBinary' uses 'fromBinaryAs' and 'toBinaryAs' in its
--    'FromJSON' instance and 'ToJSON' instance respectively.
--    For example:
--
--    >>> Data.Aeson.encode (AsBinary [pstr|foo/bar|])
--    "[102,111,111,47,98,97,114]"
--
--  * A path wrapped in a 'AsTextual' uses 'fromTextualAs' and 'unsafeToTextual'
--    in its 'FromJSON' instance and 'ToJSON' instance respectively.
--    The encoding used is determined by the type parameter of 'AsTextual', i.e.
--    a @AsTextual enc PLATFORM_STRING@ will be encoded to the textual
--    representation using the encoding @enc@.

----------------------------------------
-- Base64
----------------------------------------

instance FromJSON (As Base64 PLATFORM_STRING) where
    parseJSON = fromBase64As
    {-# INLINE parseJSON #-}

instance ToJSON (As Base64 PLATFORM_STRING) where
    toJSON = toBase64As
    {-# INLINE toJSON #-}
    toEncoding = toBase64EncodingAs
    {-# INLINE toEncoding #-}

instance FromJSONKey (As Base64 PLATFORM_STRING) where
    -- TODO: Use FromJSONKeyTextParser here
    fromJSONKey = Data.Aeson.Types.FromJSONKeyValue parseJSON
    {-# INLINE fromJSONKey #-}

instance ToJSONKey (As Base64 PLATFORM_STRING) where
    -- TODO: Use toJSONKeyText here
    toJSONKey = Data.Aeson.Types.ToJSONKeyValue toJSON toEncoding
    {-# INLINE toJSONKey #-}

----------------------------------------
-- Binary
----------------------------------------

instance FromJSON (As Binary PLATFORM_STRING) where
    parseJSON = fromBinaryAs
    {-# INLINE parseJSON #-}

instance ToJSON (As Binary PLATFORM_STRING) where
    toJSON = toBinaryAs
    {-# INLINE toJSON #-}
    toEncoding = toBinaryEncodingAs
    {-# INLINE toEncoding #-}

instance FromJSONKey (As Binary PLATFORM_STRING) where
    fromJSONKey = Data.Aeson.Types.FromJSONKeyValue parseJSON
    {-# INLINE fromJSONKey #-}

instance ToJSONKey (As Binary PLATFORM_STRING) where
    toJSONKey = Data.Aeson.Types.ToJSONKeyValue toJSON toEncoding
    {-# INLINE toJSONKey #-}

----------------------------------------
-- Textual
----------------------------------------

instance
    (IsTextEncoding enc)
    => FromJSON (As (Textual enc) PLATFORM_STRING)
    where
    parseJSON = fromTextualAs
    {-# INLINE parseJSON #-}

instance (IsTextEncoding enc) => ToJSON (As (Textual enc) PLATFORM_STRING) where
    toJSON = unsafeToTextualAs
    {-# INLINE toJSON #-}
    toEncoding = unsafeToTextualEncodingAs
    {-# INLINE toEncoding #-}

instance
    (IsTextEncoding enc)
    => FromJSONKey (As (Textual enc) PLATFORM_STRING)
    where
    fromJSONKey =
        Data.Aeson.Types.FromJSONKeyTextParser
            (fmap As . unsafeEncodeWith (textEncoding @enc) . Text.unpack)
    {-# INLINE fromJSONKey #-}

instance
    (IsTextEncoding enc)
    => ToJSONKey (As (Textual enc) PLATFORM_STRING)
    where
    toJSONKey =
        Data.Aeson.Types.toJSONKeyText
            (Text.pack . unsafeDecodeWith (textEncoding @enc) . unAs)
    {-# INLINE toJSONKey #-}

----------------------------------------
-- Tagged
----------------------------------------

instance
    (FromJSON (As t PLATFORM_STRING), Typeable t)
    => FromJSON (As (Tagged t) PLATFORM_STRING)
    where
    parseJSON = fromTaggedAs parseJSON
    {-# INLINE parseJSON #-}

instance
    (ToJSON (As t PLATFORM_STRING), Typeable t)
    => ToJSON (As (Tagged t) PLATFORM_STRING)
    where
    toJSON = toTaggedAs toJSON
    {-# INLINE toJSON #-}
    toEncoding = toTaggedEncodingAs toEncoding
    {-# INLINE toEncoding #-}

instance
    (FromJSON (As t PLATFORM_STRING), Typeable t)
    => FromJSONKey (As (Tagged t) PLATFORM_STRING)
    where
    fromJSONKey = Data.Aeson.Types.FromJSONKeyValue parseJSON
    {-# INLINE fromJSONKey #-}

instance
    (ToJSON (As t PLATFORM_STRING), Typeable t)
    => ToJSONKey (As (Tagged t) PLATFORM_STRING)
    where
    toJSONKey = Data.Aeson.Types.ToJSONKeyValue toJSON toEncoding
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
