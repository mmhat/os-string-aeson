-- TODO: Module header here

{-# LANGUAGE CPP #-}

#if defined(mingw32_HOST_OS)
#define BASE64_EXAMPLE "\"ZgBvAG8ALwBiAGEAcgA=\""
#else
#define BASE64_EXAMPLE "\"Zm9vL2Jhcg==\""
#endif

-- | As laid out in [this blog post](https://hasufell.github.io/posts/2024-05-07-ultimate-string-guide.html#to-json),
-- there are several possible encodings for an 'OsString' in JSON.
-- This library provides the boilerplate for three basic encodings:
--
-- * The /base64/ representation encodes\/decodes an 'OsString' as a
--   base64-encoded string in JSON:
--
--     >>> Data.Aeson.encode (toBase64 [osstr|foo/bar|])
--     BASE64_EXAMPLE
--
--     >>> Data.Aeson.Types.parseMaybe fromBase64 =<< Data.Aeson.decode BASE64_EXAMPLE
--     Just "foo/bar"
--
--     Note that this is a total encoding: Encoding never fails and so does
--     decoding, provided that the JSON string is a valid base64 encoding.
--
-- * The /binary/ representation encodes\/decodes an 'OsString' as a
--   sequence of numbers in JSON, where each number represents the numeric
--   encoding of one character:
--
--     >>> Data.Aeson.encode (toBinary [osstr|foo/bar|])
--     "[102,111,111,47,98,97,114]"
--
--     >>> Data.Aeson.Types.parseMaybe fromBinary =<< Data.Aeson.decode "[102,111,111,47,98,97,114]"
--     Just "foo/bar"
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
--     >>> Data.Aeson.encode <$> toTextualWith unicode [osstr|foo/bar|]
--     "\"foo/bar\""
--
--     >>> Data.Aeson.Types.parseMaybe (fromTextualWith unicode) =<< Data.Aeson.decode "\"foo/bar\""
--     Just "foo/bar"
--
--     Other functions expect that the encoding is passed on the type-level
--     (you need the @TypeApplications@ language extensions for this to work):
--
--     >>> Data.Aeson.encode <$> toTextual @Unicode [osstr|foo/bar|]
--     "\"foo/bar\""
--
--     >>> Data.Aeson.Types.parseMaybe (fromTextual @Unicode) =<< Data.Aeson.decode "\"foo/bar\""
--     Just "foo/bar"
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
--     >>> Data.Aeson.encode <$> toTaggedM (toTextualAs @Utf8) [osstr|foo/bar|]
--     "{\"data\":\"foo/bar\",\"platform\":\"Posix\"}"
--
--     >>> Data.Aeson.Types.parseMaybe (fromTagged (fromTextualAs @Utf8)) =<< Data.Aeson.decode "{\"platform\": \"Posix\", \"data\": \"foo/bar\"}"
--     Just "foo/bar"
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
module System.OsString.Aeson (
    -- * Conversion functions
    defaultParseJSON,
    defaultToJSON,
    defaultToEncoding,
    fromBase64,
    fromBase64As,
    fromBinary,
    fromBinaryAs,
    fromTextual,
    fromTextualAs,
    fromTextualWith,
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
    toTextual,
    toTextualAs,
    toTextualWith,
    toTextualEncoding,
    toTextualEncodingAs,
    toTextualEncodingWith,
    toTagged,
    toTaggedAs,
    toTaggedM,
    toTaggedAsM,
    toTaggedEncoding,
    toTaggedEncodingAs,
    toTaggedEncodingM,
    toTaggedEncodingAsM,
    unsafeToTextual,
    unsafeToTextualAs,
    unsafeToTextualWith,
    unsafeToTextualEncoding,
    unsafeToTextualEncodingAs,
    unsafeToTextualEncodingWith,

    -- * Conversion using newtype wrappers
    As (
        As,
        AsBase64,
        AsBinary,
        AsTextual,
        AsTaggedBase64,
        AsTaggedBinary,
        AsTaggedTextual
    ),
    Base64,
    Binary,
    Textual,
    Tagged,
    asBase64,
    asBinary,
    asTextual,
    asTaggedBase64,
    asTaggedBinary,
    asTaggedTextual,

    -- * Text encodings
    TextEncoding,
    IsTextEncoding,
    Unicode,
    Utf8,
    Utf16LE,
    unicode,
)
where

import System.IO (TextEncoding)
import System.OsString.Aeson.Internal
import System.OsString.Aeson.Internal.Types

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :set -XQuasiQuotes
-- >>> :set -XTypeApplications
-- >>> import Data.Aeson qualified
-- >>> import System.OsString (osstr)
