-- This template expects CPP definitions for:
--     PLATFORM_NAME = Posix | Windows
--     PLATFORM_NAME_DOUBLE = "Posix" | "Windows"
--     PLATFORM_STRING = PosixPath | WindowsPath
--     PLATFORM_CHAR = PosixChar | WindowsChar
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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
                "Binary"
                fromBinary
                [ taggedBinary "empty" []
                , taggedBinary "[\\NUL]" [0]
                ]
            , testTagged
                "Text"
                (fromText @Unicode)
                [ taggedText "empty" ""
                , taggedText "\"\\NUL\"" "\NUL"
                ]
            ]
        ]

testTagged
    :: forall (t :: Tag 'Nested)
     . (Typeable t, Typeable (TagEncoding t))
    => TestName
    -> (Value -> Parser (As t PLATFORM_STRING))
    -> [TestTree]
    -> TestTree
testTagged name parse others =
    testGroup name $
        [ unit_tagged_noPlatform parse
        , unit_tagged_noEncoding parse
        , unit_tagged_noPayload parse
        ]
            <> others

unit_tagged_noPlatform
    :: forall (t :: Tag 'Nested)
     . (Typeable t, Typeable (TagEncoding t))
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
    :: forall (t :: Tag 'Nested)
     . (Typeable t, Typeable (TagEncoding t))
    => (Value -> Parser (As t PLATFORM_STRING))
    -> TestTree
unit_tagged_noEncoding parse = testCase "no encoding" $ do
    let
        value =
            Aeson.object
                [ "platform" .= (PLATFORM_NAME_DOUBLE :: Text)
                , "payload" .= ([] :: [Word])
                ]
    let
        actual = Aeson.parseMaybe (fromTagged parse) value
    assertBool "parse should fail" (isNothing actual)

unit_tagged_noPayload
    :: forall (t :: Tag 'Nested)
     . (Typeable t, Typeable (TagEncoding t))
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

taggedBinary :: TestName -> [Word] -> TestTree
taggedBinary = taggedHelper fromBinary

taggedText :: TestName -> Text -> TestTree
taggedText = taggedHelper (fromText @Unicode)

taggedHelper
    :: forall (t :: Tag 'Nested) a
     . (ToJSON a, Typeable t, Typeable (TagEncoding t))
    => (Value -> Parser (As t PLATFORM_STRING))
    -> TestName
    -> a
    -> TestTree
taggedHelper parse name payload = testCase name $ do
    let
        value =
            Aeson.object
                [ "platform" .= (PLATFORM_NAME_DOUBLE :: Text)
                , "encoding" .= show (typeRep @(TagEncoding t))
                , "payload" .= payload
                ]
    let
        actual = Aeson.parseMaybe (fromTagged parse) value
    assertBool "parse should succeed" (isJust actual)
