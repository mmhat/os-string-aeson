{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module System.OsString.Aeson.Internal where

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
import System.OsString.Aeson.Internal.Types
import System.OsString.Internal.Types (OsString (..), PlatformString)

#if defined(mingw32_HOST_OS)
import System.OsString.Aeson.Internal.Windows qualified as Platform
#else
import System.OsString.Aeson.Internal.Posix qualified as Platform
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
-- Base64
----------------------------------------

fromBase64 :: Value -> Parser OsString
fromBase64 = fmap OsString . Platform.fromBase64
{-# INLINE fromBase64 #-}

fromBase64As :: Value -> Parser (As Base64 OsString)
fromBase64As = fmap (fmap OsString) . Platform.fromBase64As
{-# INLINE fromBase64As #-}

toBase64 :: OsString -> Value
toBase64 = Platform.toBase64 . getOsString
{-# INLINE toBase64 #-}

toBase64As :: As Base64 OsString -> Value
toBase64As = Platform.toBase64As . fmap getOsString
{-# INLINE toBase64As #-}

toBase64Encoding :: OsString -> Encoding
toBase64Encoding = Platform.toBase64Encoding . getOsString
{-# INLINE toBase64Encoding #-}

toBase64EncodingAs :: As Base64 OsString -> Encoding
toBase64EncodingAs = Platform.toBase64EncodingAs . fmap getOsString
{-# INLINE toBase64EncodingAs #-}

----------------------------------------
-- Binary
----------------------------------------

fromBinary :: Value -> Parser OsString
fromBinary = fmap OsString . Platform.fromBinary
{-# INLINE fromBinary #-}

fromBinaryAs :: Value -> Parser (As Binary OsString)
fromBinaryAs = fmap (fmap OsString) . Platform.fromBinaryAs
{-# INLINE fromBinaryAs #-}

toBinary :: OsString -> Value
toBinary = Platform.toBinary . getOsString
{-# INLINE toBinary #-}

toBinaryAs :: As Binary OsString -> Value
toBinaryAs = Platform.toBinaryAs . fmap getOsString
{-# INLINE toBinaryAs #-}

toBinaryEncoding :: OsString -> Encoding
toBinaryEncoding = Platform.toBinaryEncoding . getOsString
{-# INLINE toBinaryEncoding #-}

toBinaryEncodingAs :: As Binary OsString -> Encoding
toBinaryEncodingAs = Platform.toBinaryEncodingAs . fmap getOsString
{-# INLINE toBinaryEncodingAs #-}

----------------------------------------
-- Textual
----------------------------------------

fromTextual :: forall enc. (IsTextEncoding enc) => Value -> Parser OsString
fromTextual = fmap OsString . Platform.fromTextual @enc
{-# INLINE fromTextual #-}

fromTextualAs :: (IsTextEncoding enc) => Value -> Parser (As (Textual enc) OsString)
fromTextualAs = fmap (fmap OsString) . Platform.fromTextualAs
{-# INLINE fromTextualAs #-}

fromTextualWith :: TextEncoding -> Value -> Parser OsString
fromTextualWith enc = fmap OsString . Platform.fromTextualWith enc
{-# INLINE fromTextualWith #-}

toTextual
    :: forall enc m
     . (IsTextEncoding enc, MonadThrow m)
    => OsString
    -> m Value
toTextual = Platform.toTextual @enc . getOsString
{-# INLINE toTextual #-}

toTextualAs
    :: (IsTextEncoding enc, MonadThrow m)
    => As (Textual enc) OsString
    -> m Value
toTextualAs = Platform.toTextualAs . fmap getOsString
{-# INLINE toTextualAs #-}

toTextualWith :: (MonadThrow m) => TextEncoding -> OsString -> m Value
toTextualWith enc = Platform.toTextualWith enc . getOsString
{-# INLINE toTextualWith #-}

unsafeToTextual :: forall enc. (IsTextEncoding enc) => OsString -> Value
unsafeToTextual = Platform.unsafeToTextual @enc . getOsString
{-# INLINE unsafeToTextual #-}

unsafeToTextualAs :: (IsTextEncoding enc) => As (Textual enc) OsString -> Value
unsafeToTextualAs = Platform.unsafeToTextualAs . fmap getOsString
{-# INLINE unsafeToTextualAs #-}

unsafeToTextualWith :: TextEncoding -> OsString -> Value
unsafeToTextualWith enc = Platform.unsafeToTextualWith enc . getOsString
{-# INLINE unsafeToTextualWith #-}

toTextualEncoding
    :: forall enc m
     . (IsTextEncoding enc, MonadThrow m)
    => OsString
    -> m Encoding
toTextualEncoding = Platform.toTextualEncoding @enc . getOsString
{-# INLINE toTextualEncoding #-}

toTextualEncodingAs
    :: (IsTextEncoding enc, MonadThrow m)
    => As (Textual enc) OsString
    -> m Encoding
toTextualEncodingAs = Platform.toTextualEncodingAs . fmap getOsString
{-# INLINE toTextualEncodingAs #-}

toTextualEncodingWith
    :: (MonadThrow m)
    => TextEncoding
    -> OsString
    -> m Encoding
toTextualEncodingWith enc = Platform.toTextualEncodingWith enc . getOsString
{-# INLINE toTextualEncodingWith #-}

unsafeToTextualEncoding
    :: forall enc
     . (IsTextEncoding enc)
    => OsString
    -> Encoding
unsafeToTextualEncoding = Platform.unsafeToTextualEncoding @enc . getOsString
{-# INLINE unsafeToTextualEncoding #-}

unsafeToTextualEncodingAs
    :: (IsTextEncoding enc)
    => As (Textual enc) OsString
    -> Encoding
unsafeToTextualEncodingAs = Platform.unsafeToTextualEncodingAs . fmap getOsString
{-# INLINE unsafeToTextualEncodingAs #-}

unsafeToTextualEncodingWith :: TextEncoding -> OsString -> Encoding
unsafeToTextualEncodingWith enc =
    Platform.unsafeToTextualEncodingWith enc . getOsString
{-# INLINE unsafeToTextualEncodingWith #-}

----------------------------------------
-- Tagged
----------------------------------------

fromTagged
    :: (Typeable t)
    => (Value -> Parser (As t OsString))
    -> Value
    -> Parser OsString
fromTagged f = fmap OsString . Platform.fromTagged (fmap (fmap getOsString) . f)
{-# INLINE fromTagged #-}

fromTaggedAs
    :: (Typeable t)
    => (Value -> Parser (As t OsString))
    -> Value
    -> Parser (As (Tagged t) OsString)
fromTaggedAs f =
    fmap (fmap OsString) . Platform.fromTaggedAs (fmap (fmap getOsString) . f)
{-# INLINE fromTaggedAs #-}

toTagged :: (As t OsString -> Value) -> OsString -> Value
toTagged f = Platform.toTagged (f . fmap OsString) . getOsString
{-# INLINE toTagged #-}

toTaggedAs
    :: (As t OsString -> Value)
    -> As (Tagged t) OsString
    -> Value
toTaggedAs f = Platform.toTaggedAs (f . fmap OsString) . fmap getOsString
{-# INLINE toTaggedAs #-}

toTaggedM
    :: (MonadThrow m)
    => (As t OsString -> m Value)
    -> OsString
    -> m Value
toTaggedM f = Platform.toTaggedM (f . fmap OsString) . getOsString
{-# INLINE toTaggedM #-}

toTaggedAsM
    :: (MonadThrow m)
    => (As t OsString -> m Value)
    -> As (Tagged t) OsString
    -> m Value
toTaggedAsM f = Platform.toTaggedAsM (f . fmap OsString) . fmap getOsString
{-# INLINE toTaggedAsM #-}

toTaggedEncoding
    :: (As t OsString -> Encoding)
    -> OsString
    -> Encoding
toTaggedEncoding f = Platform.toTaggedEncoding (f . fmap OsString) . getOsString
{-# INLINE toTaggedEncoding #-}

toTaggedEncodingAs
    :: (As t OsString -> Encoding)
    -> As (Tagged t) OsString
    -> Encoding
toTaggedEncodingAs f =
    Platform.toTaggedEncodingAs (f . fmap OsString) . fmap getOsString
{-# INLINE toTaggedEncodingAs #-}

toTaggedEncodingM
    :: (MonadThrow m)
    => (As t OsString -> m Encoding)
    -> OsString
    -> m Encoding
toTaggedEncodingM f =
    Platform.toTaggedEncodingM (f . fmap OsString) . getOsString
{-# INLINE toTaggedEncodingM #-}

toTaggedEncodingAsM
    :: (MonadThrow m)
    => (As t OsString -> m Encoding)
    -> As (Tagged t) OsString
    -> m Encoding
toTaggedEncodingAsM f =
    Platform.toTaggedEncodingAsM (f . fmap OsString) . fmap getOsString
{-# INLINE toTaggedEncodingAsM #-}

--------------------------------------------------------------------------------
-- Default instances
--------------------------------------------------------------------------------

deriving via (As (Textual Platform.Unicode) OsString) instance FromJSON OsString

deriving via (As (Textual Platform.Unicode) OsString) instance ToJSON OsString

deriving via
    (As (Textual Platform.Unicode) OsString)
    instance
        FromJSONKey OsString

deriving via (As (Textual Platform.Unicode) OsString) instance ToJSONKey OsString

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
