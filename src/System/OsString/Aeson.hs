-- TODO: Module header here

-- | As laid out in [this blog post](https://hasufell.github.io/posts/2024-05-07-ultimate-string-guide.html#to-json),
-- there are several possible encodings for an 'OsString' in JSON.
-- This library provides the boilerplate for three basic encodings:
--
-- * The /base64/ representation encodes\/decodes an 'OsString' as a
--   base64-encoded string in JSON:
--
--     >>> Data.Aeson.encode (toBase64 [osstr|foo/bar|])
--     "Zm9vL2Jhcg=="
--
--     >>> Data.Aeson.Types.parseMaybe fromBase64 =<< Data.Aeson.decode "\"Zm9vL2Jhcg==\""
--     Just [osstr|foo/bar|]
--
--     Note that this is a total encoding: Encoding never fails and so does
--     decoding, provided that the JSON string is a valid base64 encoding.
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
--     >>> Data.Aeson.Types.parseMaybe (fromTextWith unicode) =<< Data.Aeson.decode "\"foo/bar\""
--     Just [osstr|foo/bar|]
--
--     Other functions expect that the encoding is passed on the type-level
--     (you need the @TypeApplications@ language extensions for this to work):
--
--     >>> Data.Aeson.encode <$> toText @Unicode [osstr|foo/bar|]
--     "\"foo/bar\""
--
--     >>> Data.Aeson.Types.parseMaybe (fromText @Unicode) =<< Data.Aeson.decode "\"foo/bar\""
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
-- In addition to the three basic representations above there is a /tagged/
-- flavour for each of those: That is, the array of numbers or string is wrapped
-- in an object that provides additional information about the 'OsString'. For
-- example:
--
--     >>> Data.Aeson.encode <$> toTaggedM (toTextAs @Utf8) [osstr|foo/bar|]
--     "{\"platform\": \"Posix\", \"data\": \"foo/bar\"}"
--
--     >>> parseMaybe (fromTagged (fromTextAs @Utf8)) =<< Data.Aeson.decode "{\"platform\": \"Posix\", \"data\": \"foo/bar\"}"
--     Just [osstr|foo/bar|]
--
-- Tagging an 'OsString' tries to solve the following issues of the basic representations:
--
-- * It prevents for example that an encoded 'System.OsString.Posix.PosixPath'
--   is mistakenly interpreted as 'System.OsString.Windows.WindowsString'.
--
-- * It prevents that a base64-encoded string is interpreted as plain text value.
--
-- Both "System.OsString.Aeson.Posix" and "System.OsString.Aeson.Windows"
-- provide the same interface as this module, but for
-- 'System.OsString.Posix.PosixString' and
-- 'System.OsString.Windows.WindowsString' respectively.
module System.OsString.Aeson
  ( -- * Conversion functions
    defaultParseJSON,
    defaultToJSON,
    defaultToEncoding,
    fromBase64,
    fromBase64As,
    fromBinary,
    fromBinaryAs,
    fromText,
    fromTextAs,
    fromTextWith,
    fromTagged,
    fromTaggedAs,
    toBase64,
    toBase64As,
    toBase64Encoding,
    toBase64EncodingAs,
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
    As
      ( As,
        AsBase64,
        AsBinary,
        AsText,
        AsTaggedBase64,
        AsTaggedBinary,
        AsTaggedText
      ),
    Tag (..),
    Level (..),
    asBase64,
    asBinary,
    asText,
    asTaggedBase64,
    asTaggedBinary,
    asTaggedText,

    -- * Text encodings
    TextEncoding,
    IsTextEncoding,
    PlatformDependent.Unicode,
    Utf8,
    Utf16LE,
    PlatformDependent.unicode,
  )
where

import System.IO (TextEncoding)
import System.OsString.Aeson.Internal
import System.OsString.Aeson.Internal.Types

#if IS_WINDOWS
import System.OsString.Aeson.Internal.Windows qualified as PlatformDependent
#else
import System.OsString.Aeson.Internal.Posix qualified as PlatformDependent
#endif
