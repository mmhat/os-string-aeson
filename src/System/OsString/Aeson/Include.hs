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
module System.OsString.Aeson.PLATFORM_NAME
  ( -- * Conversion functions
    -- $funtions
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
    -- $newtypes
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

-- $functions
-- TODO

-- $newtypes
-- In addition to the conversion functions this module provides newtype wrappers
-- to control the conversion between JSON value and a PLATFORM_STRING_SINGLE:
--
--  * A path wrapped in a 'AsBinary' uses 'fromBinaryAs' and 'toBinaryAs' in its
--    'FromJSON' instance and 'ToJSON' instance respectively.
--    For example:
--
--    >>> Data.Aeson.encode (AsBinary [pstr|foo/bar|])
--    "[102,111,111,92,98,97,114]"
--
--  * A path wrapped in a 'AsText' uses 'fromTextAs' and 'unsafeToText' in its
--    'FromJSON' instance and 'ToJSON' instance respectively.
--    The encoding used is determined by the type parameter of 'AsText', i.e.
--    a @AsText enc PLATFORM_STRING@ will be encoded to the textual
--    representation using the encoding @enc@.
