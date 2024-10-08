{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module HKD1 (test) where

import Control.Monad (guard)
import Data.Aeson.Types (FromJSON (..), ToJSON (..))
import Data.Functor.Identity (Identity (..))
import GHC.Generics (Generic)
import System.OsString
import System.OsString.Aeson

import Utils

type Mapping = Mapping' Identity

data Mapping' f = Mapping
    { source :: f OsString
    , destination :: f OsString
    }
    deriving (Generic)

deriving instance Eq Mapping

deriving instance Eq (Mapping' (As Binary))

deriving instance Eq (Mapping' (As (Textual Unicode)))

deriving instance Eq (Mapping' (As (Tagged Binary)))

deriving instance Eq (Mapping' (As (Tagged (Textual Unicode))))

instance FromJSON (Mapping' (As Binary))

instance ToJSON (Mapping' (As Binary))

instance FromJSON (Mapping' (As (Textual Unicode)))

instance ToJSON (Mapping' (As (Textual Unicode)))

instance FromJSON (Mapping' (As (Tagged Binary)))

instance ToJSON (Mapping' (As (Tagged Binary)))

instance FromJSON (Mapping' (As (Tagged (Textual Unicode))))

instance ToJSON (Mapping' (As (Tagged (Textual Unicode))))

deriving via (CoercibleRep (Mapping' (As (Textual Unicode))) Mapping) instance FromJSON Mapping

deriving via (CoercibleRep (Mapping' (As (Textual Unicode))) Mapping) instance ToJSON Mapping

example :: Mapping
example =
    Mapping
        { source = Identity exampleSource
        , destination = Identity exampleDestination
        }

testBinary :: IO ()
testBinary = do
    putStrLn "## Binary"
    let
        example' :: Mapping' (As Binary)
        example' = coerceViaRep example
        json = toJSON example'
    printJSON json
    mapping <- parseThrow parseJSON json
    guard $ mapping == example'

testTextual :: IO ()
testTextual = do
    putStrLn "## Textual"
    let
        example' :: Mapping' (As (Textual Unicode))
        example' = coerceViaRep example
        json = toJSON example'
    printJSON json
    mapping <- parseThrow parseJSON json
    guard $ mapping == example'

testTaggedBinary :: IO ()
testTaggedBinary = do
    putStrLn "## Tagged Binary"
    let
        example' :: Mapping' (As (Tagged Binary))
        example' = coerceViaRep example
        json = toJSON example'
    printJSON json
    mapping <- parseThrow parseJSON json
    guard $ mapping == example'

testTaggedTextual :: IO ()
testTaggedTextual = do
    putStrLn "## Tagged Textual"
    let
        example' :: Mapping' (As (Tagged (Textual Unicode)))
        example' = coerceViaRep example
        json = toJSON example'
    printJSON json
    mapping <- parseThrow parseJSON json
    guard $ mapping == example'

testDerivedDefaultInstances :: IO ()
testDerivedDefaultInstances = do
    putStrLn "## Derived default instances"
    let
        json = toJSON example
    printJSON json
    mapping <- parseThrow parseJSON json
    guard $ mapping == example

test :: IO ()
test = do
    putStrLn "# HKD 1"
    testBinary
    testTextual
    testTaggedBinary
    testTaggedTextual
    testDerivedDefaultInstances
