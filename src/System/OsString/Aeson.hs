{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | As laid out in [this blog post](https://hasufell.github.io/posts/2024-05-07-ultimate-string-guide.html#to-json),
-- there are several possible encodings for an 'OsString' in JSON.
-- This library provides the boilerplate for two basic encodings:
--
-- * The /binary/ representation encodes\/decodes an 'OsString' as a
--   sequence of numbers in JSON, where each number represents the numeric
--   encoding of one character:
--
--     >>> Data.Aeson.encode (toBinary [osstr|foo/bar|])
--     "[102,111,111,92,98,97,114]"
--
--     >>> Data.Aeson.Types.parseMaybe fromBinary =<< Data.Aeson.decode "[102,111,111,92,98,97,114]"
--     Just [osstr|foo/bar|]
--
--     Note that this is a total encoding: Encoding never fails and so does
--     decoding, provided that the numbers are valid 'Data.Word.Word8' values
--     (if the underlying type is 'System.OsString.Posix.PosixString') or valid
--     'Data.Word.Word16' values (if the underlying type is
--     'System.OsString.Windows.WindowsString').
--
-- * The /textual/ representation tries to encode\/decode an 'OsString' as a
--   string in JSON. In order to do that we also have to provide an encoding.
--
--     Some functions in this module (those suffixed with \"With\") take a
--     'System.IO.TextEncoding' as an argument, and you may use any of those
--     defined in "System.IO" or "System.OsString.Encoding":
--
--     >>> Data.Aeson.encode <$> toTextWith unicode [osstr|foo/bar|]
--     "\"foo/bar\""
--
--     >>> parseMaybe (fromTextWith unicode) =<< Data.Aeson.decode "\"foo/bar\""
--     Just [osstr|foo/bar|]
--
--     Other functions expect that the encoding is passed on the type-level
--     (you need the @TypeApplications@ language extensions for this to work):
--
--     >>> Data.Aeson.encode <$> toText @Unicode [osstr|foo/bar|]
--     "\"foo/bar\""
--
--     >>> parseMaybe (fromText @Unicode) =<< Data.Aeson.decode "\"foo/bar\""
--     Just [osstr|foo/bar|]
--
--     This module provides the encoding types 'Utf8', 'Utf16LE' and 'Unicode',
--     where the latter one of the former two depending on the current platform.
--
--     __WARNING:__ Both decoding and encoding may fail with a
--     'System.OsString.EncodingException'.
--     The examples above work because 'osstr' encodes to the appropriate
--     Unicode encoding for the particular platform.
--
-- In addition to the two basic representations above there is a /tagged/
-- flavour of both: That is, the array of numbers or string is wrapped in an
-- object that provides additional information about the 'OsString'. For
-- example:
--
--     >>> Data.Aeson.encode <$> toTaggedM (toTextAs @Utf8) [osstr|foo/bar|]
--     "{\"platform\": \"Posix\", \"encoding\": \"Utf8\", \"payload\": \"foo/bar\"}"
--
--     >>> parseMaybe (fromTagged (fromTextAs @Utf8)) =<< Data.Aeson.decode "{\"platform\": \"Posix\", \"encoding\": \"Utf8\", \"payload\": \"foo/bar\"}"
--     Just [osstr|foo/bar|]
--
-- Tagging an 'OsString' prevents for example that an encoded
-- 'System.OsString.Posix.PosixPath' is mistakenly interpreted as
-- 'System.OsString.Windows.WindowsString'.
--
-- Both "System.OsString.Aeson.Posix" and "System.OsString.Aeson.Windows"
-- provide the same interface as this module, but for
-- 'System.OsString.Posix.PosixString' and
-- 'System.OsString.Windows.WindowsString' respectively.
module System.OsString.Aeson (
    -- * Conversion functions
    defaultParseJSON,
    defaultToJSON,
    defaultToEncoding,
    fromBinary,
    fromBinaryAs,
    fromText,
    fromTextAs,
    fromTextWith,
    fromTagged,
    fromTaggedAs,
    toBinary,
    toBinaryAs,
    toBinaryEncoding,
    toBinaryEncodingAs,
    toText,
    toTextAs,
    toTextWith,
    toTextEncoding,
    toTextEncodingAs,
    toTextEncodingWith,
    toTagged,
    toTaggedAs,
    toTaggedM,
    toTaggedAsM,
    toTaggedEncoding,
    toTaggedEncodingAs,
    toTaggedEncodingM,
    toTaggedEncodingAsM,
    unsafeToText,
    unsafeToTextAs,
    unsafeToTextWith,
    unsafeToTextEncoding,
    unsafeToTextEncodingAs,
    unsafeToTextEncodingWith,

    -- * Conversion using newtype wrappers
    As (As, AsBinary, AsText, AsTaggedBinary, AsTaggedText),
    Tag (..),
    Level (..),
    TagEncoding (..),

    -- * Text encodings
    TextEncoding,
    IsTextEncoding,
    Platform.Unicode,
    Utf8,
    Utf16LE,
    Platform.unicode,
) where

import Control.Monad.Catch (MonadThrow)
import Data.Aeson.Types (
    Encoding,
    FromJSON,
    FromJSONKey,
    Parser,
    ToJSON,
    ToJSONKey,
    Value,
 )
import Data.Typeable (Typeable)
import System.IO (TextEncoding)
import System.OsString.Internal.Types (OsString (..), PlatformString)

import System.OsString.Aeson.Types

#if defined(mingw32_HOST_OS)
import System.OsString.Aeson.Windows qualfied as Platform
#else
import System.OsString.Aeson.Posix qualified as Platform
#endif

--------------------------------------------------------------------------------
-- Conversion functions
--------------------------------------------------------------------------------

defaultParseJSON :: Value -> Parser OsString
defaultParseJSON = fmap OsString . Platform.defaultParseJSON
{-# INLINE defaultParseJSON #-}

defaultToJSON :: OsString -> Value
defaultToJSON = Platform.defaultToJSON . getOsString
{-# INLINE defaultToJSON #-}

defaultToEncoding :: OsString -> Encoding
defaultToEncoding = Platform.defaultToEncoding . getOsString
{-# INLINE defaultToEncoding #-}

----------------------------------------
-- Binary
----------------------------------------

fromBinary :: Value -> Parser OsString
fromBinary = fmap OsString . Platform.fromBinary
{-# INLINE fromBinary #-}

fromBinaryAs :: Value -> Parser (As 'Binary OsString)
fromBinaryAs = fmap (fmap OsString) . Platform.fromBinaryAs
{-# INLINE fromBinaryAs #-}

toBinary :: OsString -> Value
toBinary = Platform.toBinary . getOsString
{-# INLINE toBinary #-}

toBinaryAs :: As 'Binary OsString -> Value
toBinaryAs = Platform.toBinaryAs . fmap getOsString
{-# INLINE toBinaryAs #-}

toBinaryEncoding :: OsString -> Encoding
toBinaryEncoding = Platform.toBinaryEncoding . getOsString
{-# INLINE toBinaryEncoding #-}

toBinaryEncodingAs :: As 'Binary OsString -> Encoding
toBinaryEncodingAs = Platform.toBinaryEncodingAs . fmap getOsString
{-# INLINE toBinaryEncodingAs #-}

----------------------------------------
-- Text
----------------------------------------

fromText :: forall enc. (IsTextEncoding enc) => Value -> Parser OsString
fromText = fmap OsString . Platform.fromText @enc
{-# INLINE fromText #-}

fromTextAs :: (IsTextEncoding enc) => Value -> Parser (As ('Text enc) OsString)
fromTextAs = fmap (fmap OsString) . Platform.fromTextAs
{-# INLINE fromTextAs #-}

fromTextWith :: TextEncoding -> Value -> Parser OsString
fromTextWith enc = fmap OsString . Platform.fromTextWith enc
{-# INLINE fromTextWith #-}

toText
    :: forall enc m
     . (IsTextEncoding enc, MonadThrow m)
    => OsString -> m Value
toText = Platform.toText @enc . getOsString
{-# INLINE toText #-}

toTextAs
    :: (IsTextEncoding enc, MonadThrow m)
    => As ('Text enc) OsString -> m Value
toTextAs = Platform.toTextAs . fmap getOsString
{-# INLINE toTextAs #-}

toTextWith :: (MonadThrow m) => TextEncoding -> OsString -> m Value
toTextWith enc = Platform.toTextWith enc . getOsString
{-# INLINE toTextWith #-}

unsafeToText :: forall enc. (IsTextEncoding enc) => OsString -> Value
unsafeToText = Platform.unsafeToText @enc . getOsString
{-# INLINE unsafeToText #-}

unsafeToTextAs :: (IsTextEncoding enc) => As ('Text enc) OsString -> Value
unsafeToTextAs = Platform.unsafeToTextAs . fmap getOsString
{-# INLINE unsafeToTextAs #-}

unsafeToTextWith :: TextEncoding -> OsString -> Value
unsafeToTextWith enc = Platform.unsafeToTextWith enc . getOsString
{-# INLINE unsafeToTextWith #-}

toTextEncoding
    :: forall enc m
     . (IsTextEncoding enc, MonadThrow m)
    => OsString -> m Encoding
toTextEncoding = Platform.toTextEncoding @enc . getOsString
{-# INLINE toTextEncoding #-}

toTextEncodingAs
    :: (IsTextEncoding enc, MonadThrow m)
    => As ('Text enc) OsString -> m Encoding
toTextEncodingAs = Platform.toTextEncodingAs . fmap getOsString
{-# INLINE toTextEncodingAs #-}

toTextEncodingWith :: (MonadThrow m) => TextEncoding -> OsString -> m Encoding
toTextEncodingWith enc = Platform.toTextEncodingWith enc . getOsString
{-# INLINE toTextEncodingWith #-}

unsafeToTextEncoding :: forall enc. (IsTextEncoding enc) => OsString -> Encoding
unsafeToTextEncoding = Platform.unsafeToTextEncoding @enc . getOsString
{-# INLINE unsafeToTextEncoding #-}

unsafeToTextEncodingAs
    :: (IsTextEncoding enc)
    => As ('Text enc) OsString -> Encoding
unsafeToTextEncodingAs = Platform.unsafeToTextEncodingAs . fmap getOsString
{-# INLINE unsafeToTextEncodingAs #-}

unsafeToTextEncodingWith :: TextEncoding -> OsString -> Encoding
unsafeToTextEncodingWith enc =
    Platform.unsafeToTextEncodingWith enc . getOsString
{-# INLINE unsafeToTextEncodingWith #-}

----------------------------------------
-- Tagged
----------------------------------------

fromTagged
    :: (TagEncoding t, Typeable t)
    => (Value -> Parser (As (t :: Tag 'Nested) OsString))
    -> Value
    -> Parser OsString
fromTagged f = fmap OsString . Platform.fromTagged (fmap (fmap getOsString) . f)
{-# INLINE fromTagged #-}

fromTaggedAs
    :: (TagEncoding t, Typeable t)
    => (Value -> Parser (As (t :: Tag 'Nested) OsString))
    -> Value
    -> Parser (As ('Tagged t) OsString)
fromTaggedAs f =
    fmap (fmap OsString) . Platform.fromTaggedAs (fmap (fmap getOsString) . f)
{-# INLINE fromTaggedAs #-}

toTagged
    :: (TagEncoding t)
    => (As (t :: Tag 'Nested) OsString -> Value) -> OsString -> Value
toTagged f = Platform.toTagged (f . fmap OsString) . getOsString
{-# INLINE toTagged #-}

toTaggedAs
    :: (TagEncoding t)
    => (As (t :: Tag 'Nested) OsString -> Value)
    -> As ('Tagged t) OsString
    -> Value
toTaggedAs f = Platform.toTaggedAs (f . fmap OsString) . fmap getOsString
{-# INLINE toTaggedAs #-}

toTaggedM
    :: (MonadThrow m, TagEncoding t)
    => (As (t :: Tag 'Nested) OsString -> m Value) -> OsString -> m Value
toTaggedM f = Platform.toTaggedM (f . fmap OsString) . getOsString
{-# INLINE toTaggedM #-}

toTaggedAsM
    :: (MonadThrow m, TagEncoding t)
    => (As t OsString -> m Value) -> As ('Tagged t) OsString -> m Value
toTaggedAsM f = Platform.toTaggedAsM (f . fmap OsString) . fmap getOsString
{-# INLINE toTaggedAsM #-}

toTaggedEncoding
    :: (TagEncoding t)
    => (As (t :: Tag 'Nested) OsString -> Encoding) -> OsString -> Encoding
toTaggedEncoding f = Platform.toTaggedEncoding (f . fmap OsString) . getOsString
{-# INLINE toTaggedEncoding #-}

toTaggedEncodingAs
    :: (TagEncoding t)
    => (As (t :: Tag 'Nested) OsString -> Encoding)
    -> As ('Tagged t) OsString
    -> Encoding
toTaggedEncodingAs f =
    Platform.toTaggedEncodingAs (f . fmap OsString) . fmap getOsString
{-# INLINE toTaggedEncodingAs #-}

toTaggedEncodingM
    :: (MonadThrow m, TagEncoding t)
    => (As (t :: Tag 'Nested) OsString -> m Encoding) -> OsString -> m Encoding
toTaggedEncodingM f =
    Platform.toTaggedEncodingM (f . fmap OsString) . getOsString
{-# INLINE toTaggedEncodingM #-}

toTaggedEncodingAsM
    :: (MonadThrow m, TagEncoding t)
    => (As t OsString -> m Encoding)
    -> As ('Tagged t) OsString
    -> m Encoding
toTaggedEncodingAsM f =
    Platform.toTaggedEncodingAsM (f . fmap OsString) . fmap getOsString
{-# INLINE toTaggedEncodingAsM #-}

--------------------------------------------------------------------------------
-- Default instances
--------------------------------------------------------------------------------

deriving via (As ('Text Platform.Unicode) OsString) instance FromJSON OsString

deriving via (As ('Text Platform.Unicode) OsString) instance ToJSON OsString

deriving via
    (As ('Text Platform.Unicode) OsString)
    instance
        FromJSONKey OsString

deriving via (As ('Text Platform.Unicode) OsString) instance ToJSONKey OsString

--------------------------------------------------------------------------------
-- Instances for newtype wrappers
--------------------------------------------------------------------------------

deriving via
    (As t PlatformString)
    instance
        (FromJSON (As t PlatformString))
        => FromJSON (As t OsString)

deriving via
    (As t PlatformString)
    instance
        (ToJSON (As t PlatformString))
        => ToJSON (As t OsString)

deriving via
    (As t PlatformString)
    instance
        (FromJSONKey (As t PlatformString))
        => FromJSONKey (As t OsString)

deriving via
    (As t PlatformString)
    instance
        (ToJSONKey (As t PlatformString))
        => ToJSONKey (As t OsString)
