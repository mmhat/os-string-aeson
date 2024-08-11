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
{-# LANGUAGE CPP #-}

-- | This module provides the same interface as 'System.OsString.Aeson', but for
-- PLATFORM_STRING_SINGLE. Please see the documentation of
-- 'System.OsString.Aeson' on how to use this module.
module System.OsString.Aeson.PLATFORM_NAME (
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
    IsTextEncoding (..),
    Unicode,
    Utf8,
    Utf16LE,
    unicode,
)
where

import System.IO (TextEncoding)
import System.OsString.Aeson.Internal.Types

#if IS_WINDOWS
import System.OsString.Aeson.Internal.Windows
#else
import System.OsString.Aeson.Internal.Posix
#endif
