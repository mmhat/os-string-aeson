-- This template expects CPP definitions for:
--     PLATFORM_NAME = Posix | Windows
--     PLATFORM_STRING = PosixPath | WindowsPath
--     PLATFORM_CHAR = PosixChar | WindowsChar

#define PLATFORM_NAME_DOUBLE " ## PLATFORM_NAME ## "

{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module PLATFORM_NAME (
    tests,
) where

import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Parser)
import Data.Aeson.Types qualified as Aeson
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.OsString.Internal.Types (PLATFORM_CHAR (..))
import System.OsString.PLATFORM_NAME (PLATFORM_STRING)
import System.OsString.PLATFORM_NAME qualified as OsString
import Test.Tasty
import Test.Tasty.Hedgehog

import System.OsString.Aeson.PLATFORM_NAME

tests :: TestTree
tests =
    testGroup
        PLATFORM_NAME_DOUBLE
        [ test_properties
        ]

test_properties :: TestTree
test_properties = fromGroup $$(discover)

prop_functions_roundtrip :: Property
prop_functions_roundtrip =
    roundtripFunctions genUnrestricted fromBinary toBinary

prop_instances_roundtrip :: Property
prop_instances_roundtrip =
    roundtripInstances genUnicode id

prop_instances_roundtripBinary :: Property
prop_instances_roundtripBinary =
    roundtripInstances genUnrestricted AsBinary

prop_instances_roundtripUnicode :: Property
prop_instances_roundtripUnicode =
    roundtripInstances genUnicode (AsText @Unicode)

roundtripFunctions
    :: Gen PLATFORM_STRING
    -> (Value -> Parser (As t PLATFORM_STRING))
    -> (As t PLATFORM_STRING -> Value)
    -> Property
roundtripFunctions gen decode encode = property $ do
    string <- forAll gen
    let
        actual = Aeson.parseMaybe (decode . encode) (As string)
        expected = Just (As string)
    actual === expected

roundtripInstances
    :: (Eq a, Show a, FromJSON a, ToJSON a)
    => Gen PLATFORM_STRING
    -> (PLATFORM_STRING -> a)
    -> Property
roundtripInstances gen f = property $ do
    string <- forAll gen
    let
        wrapped = f string
        actual = Aeson.parseMaybe (Aeson.parseJSON . Aeson.toJSON) wrapped
        expected = Just wrapped
    actual === expected

genUnicode :: (MonadGen m) => m PLATFORM_STRING
genUnicode =
    OsString.unsafeEncodeUtf
        <$> Gen.list
            (Range.linear 0 100)
            Gen.unicode

genUnrestricted :: (MonadGen m) => m PLATFORM_STRING
genUnrestricted =
    OsString.pack
        <$> Gen.list
            (Range.linear 0 100)
            (PLATFORM_CHAR <$> Gen.enumBounded)
