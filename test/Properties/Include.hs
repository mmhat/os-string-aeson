-- This template expects CPP definitions for:
--     PLATFORM_NAME = Posix | Windows
--     PLATFORM_STRING = PosixPath | WindowsPath
--     PLATFORM_CHAR = PosixChar | WindowsChar
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Properties.PLATFORM_NAME (
    tests,
)
where

import Control.Monad ((<=<))
import Control.Monad.Catch (MonadThrow)
import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Parser)
import Data.Aeson.Types qualified as Aeson
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.OsString.Aeson.PLATFORM_NAME
import System.OsString.Internal.Types (PLATFORM_CHAR (..))
import System.OsString.PLATFORM_NAME (PLATFORM_STRING)
import System.OsString.PLATFORM_NAME qualified as OsString
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = fromGroup $$(discover)

prop_functions_roundtripDefault :: Property
prop_functions_roundtripDefault =
    roundtripFunctionsNoAs genUnrestricted defaultParseJSON defaultToJSON

prop_functions_roundtripBinary :: Property
prop_functions_roundtripBinary =
    roundtripFunctions genUnrestricted fromBinaryAs toBinaryAs

prop_functions_roundtripText :: Property
prop_functions_roundtripText =
    roundtripFunctionsM @('Text Unicode) genUnicode fromTextAs toTextAs

prop_functions_roundtripTaggedBinary :: Property
prop_functions_roundtripTaggedBinary =
    roundtripFunctions
        genUnrestricted
        (fromTaggedAs fromBinaryAs)
        (toTaggedAs toBinaryAs)

prop_functions_roundtripTaggedText :: Property
prop_functions_roundtripTaggedText =
    roundtripFunctionsM @('Tagged ('Text Unicode))
        genUnicode
        (fromTaggedAs fromTextAs)
        (toTaggedAsM toTextAs)

prop_instances_roundtrip :: Property
prop_instances_roundtrip =
    roundtripInstances genUnicode id

prop_instances_roundtripBinary :: Property
prop_instances_roundtripBinary =
    roundtripInstances genUnrestricted AsBinary

prop_instances_roundtripText :: Property
prop_instances_roundtripText =
    roundtripInstances genUnicode (AsText @Unicode)

prop_instances_roundtripTaggedBinary :: Property
prop_instances_roundtripTaggedBinary =
    roundtripInstances genUnrestricted AsTaggedBinary

prop_instances_roundtripTaggedText :: Property
prop_instances_roundtripTaggedText =
    roundtripInstances genUnicode (AsTaggedText @Unicode)

roundtripFunctions
    :: Gen PLATFORM_STRING
    -> (Value -> Parser (As t PLATFORM_STRING))
    -> (As t PLATFORM_STRING -> Value)
    -> Property
roundtripFunctions gen decode encode = property $ do
    string <- As <$> forAll gen
    tripping string encode (Aeson.parseEither decode)

roundtripFunctionsNoAs
    :: Gen PLATFORM_STRING
    -> (Value -> Parser PLATFORM_STRING)
    -> (PLATFORM_STRING -> Value)
    -> Property
roundtripFunctionsNoAs gen decode encode = property $ do
    string <- forAll gen
    tripping string encode (Aeson.parseEither decode)

roundtripFunctionsM
    :: Gen PLATFORM_STRING
    -> (Value -> Parser (As t PLATFORM_STRING))
    -> (forall m. (MonadThrow m) => As t PLATFORM_STRING -> m Value)
    -> Property
roundtripFunctionsM gen decode encode = property $ do
    string <- forAll gen
    let
        actual = (Aeson.parseMaybe decode <=< encode) (As string)
        expected = Just (As string)
    actual === expected

roundtripInstances
    :: (Eq a, Show a, FromJSON a, ToJSON a)
    => Gen PLATFORM_STRING
    -> (PLATFORM_STRING -> a)
    -> Property
roundtripInstances gen f = property $ do
    string <- f <$> forAll gen
    tripping string Aeson.toJSON (Aeson.parseEither Aeson.parseJSON)

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
