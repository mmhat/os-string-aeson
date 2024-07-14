-- This template expects CPP definitions for:
--     PLATFORM_NAME = Posix | Windows
--     PLATFORM_NAME_DOUBLE = "Posix" | "Windows"
--     PLATFORM_STRING = PosixPath | WindowsPath
--     PLATFORM_CHAR = PosixChar | WindowsChar
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Units.PLATFORM_NAME (
    tests,
) where

import Data.Aeson.Types (Parser, ToJSON, Value, (.=))
import Data.Aeson.Types qualified as Aeson
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Typeable (Typeable)
import System.OsString.PLATFORM_NAME (PLATFORM_STRING)
import Test.Tasty
import Test.Tasty.HUnit
import Type.Reflection (typeRep)

import System.OsString.Aeson.PLATFORM_NAME

tests :: TestTree
tests =
    testGroup
        ("Units." <> PLATFORM_NAME_DOUBLE)
        [ testGroup
            "Tagged"
            [ testTagged
                "Base64"
                fromBase64As
                False
                Text.empty
                [ taggedBase64 "empty" ""
                , taggedBase64 "null byte" "AA=="
                ]
            , testTagged
                "Binary"
                fromBinaryAs
                False
                ([] :: [Word])
                [ taggedBinary "empty" []
                , taggedBinary "null byte" [0]
                ]
            , testTagged
                "Text"
                (fromTextAs @Unicode)
                True
                Text.empty
                [ taggedText "empty" ""
                , taggedText "null byte" "\NUL"
                ]
            ]
        ]

testTagged
    :: forall (t :: Tag 'Nested) a
     . (ToJSON a, TagEncoding t, Typeable t)
    => TestName
    -> (Value -> Parser (As t PLATFORM_STRING))
    -> Bool
    -> a
    -> [TestTree]
    -> TestTree
testTagged name parse needsEncoding emptyPayload others =
    testGroup name $
        [ unit_tagged_noPlatform parse
        , unit_tagged_noEncoding needsEncoding emptyPayload parse
        , unit_tagged_noPayload parse
        ]
            <> others

unit_tagged_noPlatform
    :: forall (t :: Tag 'Nested)
     . (TagEncoding t, Typeable t)
    => (Value -> Parser (As t PLATFORM_STRING))
    -> TestTree
unit_tagged_noPlatform parse = testCase "no platform" $ do
    let
        value =
            Aeson.object
                [ "encoding" .= show (typeRep @(TagEncoding t))
                , "payload" .= ([] :: [Word])
                ]
    let
        actual = Aeson.parseMaybe (fromTagged parse) value
    assertBool "parse should fail" (isNothing actual)

unit_tagged_noEncoding
    :: forall (t :: Tag 'Nested) a
     . (ToJSON a, TagEncoding t, Typeable t)
    => Bool
    -> a
    -> (Value -> Parser (As t PLATFORM_STRING))
    -> TestTree
unit_tagged_noEncoding needsEncoding emptyPayload parse =
    testCase "no encoding" $ do
        let
            value =
                Aeson.object
                    [ "platform" .= (PLATFORM_NAME_DOUBLE :: Text)
                    , "payload" .= emptyPayload
                    ]
        let
            actual = Aeson.parseMaybe (fromTagged parse) value
        if needsEncoding
            then assertBool "parse should fail" (isNothing actual)
            else assertBool "parse should succeed" (isJust actual)

unit_tagged_noPayload
    :: forall (t :: Tag 'Nested)
     . (TagEncoding t, Typeable t)
    => (Value -> Parser (As t PLATFORM_STRING))
    -> TestTree
unit_tagged_noPayload parse = testCase "no payload" $ do
    let
        value =
            Aeson.object
                [ "platform" .= (PLATFORM_NAME_DOUBLE :: Text)
                , "encoding" .= show (typeRep @(TagEncoding t))
                ]
    let
        actual = Aeson.parseMaybe (fromTagged parse) value
    assertBool "parse should fail" (isNothing actual)

taggedBase64 :: TestName -> Text -> TestTree
taggedBase64 = taggedHelper fromBase64As

taggedBinary :: TestName -> [Word] -> TestTree
taggedBinary = taggedHelper fromBinaryAs

taggedText :: TestName -> Text -> TestTree
taggedText = taggedHelper (fromTextAs @Unicode)

taggedHelper
    :: forall (t :: Tag 'Nested) a
     . (ToJSON a, TagEncoding t, Typeable t)
    => (Value -> Parser (As t PLATFORM_STRING))
    -> TestName
    -> a
    -> TestTree
taggedHelper parse name payload = testCase name $ do
    let
        value =
            Aeson.object
                [ "platform" .= (PLATFORM_NAME_DOUBLE :: Text)
                , "encoding" .= tagEncoding @_ @t
                , "payload" .= payload
                ]
    let
        actual = Aeson.parseMaybe (fromTagged parse) value
    assertBool "parse should succeed" (isJust actual)
