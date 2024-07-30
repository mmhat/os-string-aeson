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

module System.OsString.Aeson.Internal where

import Control.Monad.Catch (MonadThrow)
import Data.Aeson.Types
  ( Encoding,
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
import System.OsString.Aeson.Internal.Windows qualfied as Platform
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

fromBase64As :: Value -> Parser (As 'Base64 OsString)
fromBase64As = fmap (fmap OsString) . Platform.fromBase64As
{-# INLINE fromBase64As #-}

toBase64 :: OsString -> Value
toBase64 = Platform.toBase64 . getOsString
{-# INLINE toBase64 #-}

toBase64As :: As 'Base64 OsString -> Value
toBase64As = Platform.toBase64As . fmap getOsString
{-# INLINE toBase64As #-}

toBase64Encoding :: OsString -> Encoding
toBase64Encoding = Platform.toBase64Encoding . getOsString
{-# INLINE toBase64Encoding #-}

toBase64EncodingAs :: As 'Base64 OsString -> Encoding
toBase64EncodingAs = Platform.toBase64EncodingAs . fmap getOsString
{-# INLINE toBase64EncodingAs #-}

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

toText ::
  forall enc m.
  (IsTextEncoding enc, MonadThrow m) =>
  OsString ->
  m Value
toText = Platform.toText @enc . getOsString
{-# INLINE toText #-}

toTextAs ::
  (IsTextEncoding enc, MonadThrow m) =>
  As ('Text enc) OsString ->
  m Value
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

toTextEncoding ::
  forall enc m.
  (IsTextEncoding enc, MonadThrow m) =>
  OsString ->
  m Encoding
toTextEncoding = Platform.toTextEncoding @enc . getOsString
{-# INLINE toTextEncoding #-}

toTextEncodingAs ::
  (IsTextEncoding enc, MonadThrow m) =>
  As ('Text enc) OsString ->
  m Encoding
toTextEncodingAs = Platform.toTextEncodingAs . fmap getOsString
{-# INLINE toTextEncodingAs #-}

toTextEncodingWith :: (MonadThrow m) => TextEncoding -> OsString -> m Encoding
toTextEncodingWith enc = Platform.toTextEncodingWith enc . getOsString
{-# INLINE toTextEncodingWith #-}

unsafeToTextEncoding :: forall enc. (IsTextEncoding enc) => OsString -> Encoding
unsafeToTextEncoding = Platform.unsafeToTextEncoding @enc . getOsString
{-# INLINE unsafeToTextEncoding #-}

unsafeToTextEncodingAs ::
  (IsTextEncoding enc) =>
  As ('Text enc) OsString ->
  Encoding
unsafeToTextEncodingAs = Platform.unsafeToTextEncodingAs . fmap getOsString
{-# INLINE unsafeToTextEncodingAs #-}

unsafeToTextEncodingWith :: TextEncoding -> OsString -> Encoding
unsafeToTextEncodingWith enc =
  Platform.unsafeToTextEncodingWith enc . getOsString
{-# INLINE unsafeToTextEncodingWith #-}

----------------------------------------
-- Tagged
----------------------------------------

fromTagged ::
  (Typeable t) =>
  (Value -> Parser (As (t :: Tag 'Nested) OsString)) ->
  Value ->
  Parser OsString
fromTagged f = fmap OsString . Platform.fromTagged (fmap (fmap getOsString) . f)
{-# INLINE fromTagged #-}

fromTaggedAs ::
  (Typeable t) =>
  (Value -> Parser (As (t :: Tag 'Nested) OsString)) ->
  Value ->
  Parser (As ('Tagged t) OsString)
fromTaggedAs f =
  fmap (fmap OsString) . Platform.fromTaggedAs (fmap (fmap getOsString) . f)
{-# INLINE fromTaggedAs #-}

toTagged :: (As (t :: Tag 'Nested) OsString -> Value) -> OsString -> Value
toTagged f = Platform.toTagged (f . fmap OsString) . getOsString
{-# INLINE toTagged #-}

toTaggedAs ::
  (As (t :: Tag 'Nested) OsString -> Value) ->
  As ('Tagged t) OsString ->
  Value
toTaggedAs f = Platform.toTaggedAs (f . fmap OsString) . fmap getOsString
{-# INLINE toTaggedAs #-}

toTaggedM ::
  (MonadThrow m) =>
  (As (t :: Tag 'Nested) OsString -> m Value) ->
  OsString ->
  m Value
toTaggedM f = Platform.toTaggedM (f . fmap OsString) . getOsString
{-# INLINE toTaggedM #-}

toTaggedAsM ::
  (MonadThrow m) =>
  (As t OsString -> m Value) ->
  As ('Tagged t) OsString ->
  m Value
toTaggedAsM f = Platform.toTaggedAsM (f . fmap OsString) . fmap getOsString
{-# INLINE toTaggedAsM #-}

toTaggedEncoding ::
  (As (t :: Tag 'Nested) OsString -> Encoding) -> OsString -> Encoding
toTaggedEncoding f = Platform.toTaggedEncoding (f . fmap OsString) . getOsString
{-# INLINE toTaggedEncoding #-}

toTaggedEncodingAs ::
  (As (t :: Tag 'Nested) OsString -> Encoding) ->
  As ('Tagged t) OsString ->
  Encoding
toTaggedEncodingAs f =
  Platform.toTaggedEncodingAs (f . fmap OsString) . fmap getOsString
{-# INLINE toTaggedEncodingAs #-}

toTaggedEncodingM ::
  (MonadThrow m) =>
  (As (t :: Tag 'Nested) OsString -> m Encoding) ->
  OsString ->
  m Encoding
toTaggedEncodingM f =
  Platform.toTaggedEncodingM (f . fmap OsString) . getOsString
{-# INLINE toTaggedEncodingM #-}

toTaggedEncodingAsM ::
  (MonadThrow m) =>
  (As t OsString -> m Encoding) ->
  As ('Tagged t) OsString ->
  m Encoding
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
    (FromJSON (As t PlatformString)) =>
    FromJSON (As t OsString)

deriving via
  (As t PlatformString)
  instance
    (ToJSON (As t PlatformString)) =>
    ToJSON (As t OsString)

deriving via
  (As t PlatformString)
  instance
    (FromJSONKey (As t PlatformString)) =>
    FromJSONKey (As t OsString)

deriving via
  (As t PlatformString)
  instance
    (ToJSONKey (As t PlatformString)) =>
    ToJSONKey (As t OsString)
