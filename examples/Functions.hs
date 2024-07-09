{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Functions (test) where

import Control.Monad (guard)
import Data.Aeson
import Data.Aeson.Types
import System.OsString
import System.OsString.Aeson

import Utils

data Mapping = Mapping
    { source :: OsString
    , destination :: OsString
    }
    deriving (Eq, Show)

example :: Mapping
example =
    Mapping
        { source = exampleSource
        , destination = exampleDestination
        }

binaryFromJSON :: Value -> Parser Mapping
binaryFromJSON = withObject "Mapping" $ \obj -> do
    source <- fromBinary =<< obj .: "source"
    destination <- fromBinary =<< obj .: "destination"
    pure Mapping{..}

binaryToJSON :: Mapping -> Value
binaryToJSON x =
    object
        [ "source" .= toBinary (source x)
        , "destination" .= toBinary (destination x)
        ]

testBinary :: IO ()
testBinary = do
    putStrLn "## Binary"
    let
        json = binaryToJSON example
    print . encode $ json -- TODO: Use aeson-pretty
    mapping <- parseThrow binaryFromJSON json
    guard $ mapping == example
    print mapping

textualFromJSON :: Value -> Parser Mapping
textualFromJSON = withObject "Mapping" $ \obj -> do
    source <- fromText @Unicode =<< obj .: "source"
    destination <- fromText @Unicode =<< obj .: "destination"
    pure Mapping{..}

textualToJSON :: Mapping -> IO Value
textualToJSON x = do
    source' <- toText @Unicode (source x)
    destination' <- toText @Unicode (destination x)
    pure . object $
        [ "source" .= source'
        , "destination" .= destination'
        ]

testTextual :: IO ()
testTextual = do
    putStrLn "## Textual"
    json <- textualToJSON example
    print . encode $ json -- TODO: Use aeson-pretty
    mapping <- parseThrow textualFromJSON json
    guard $ mapping == example
    print mapping

taggedBinaryFromJSON :: Value -> Parser Mapping
taggedBinaryFromJSON = withObject "Mapping" $ \obj -> do
    source <- fromTagged fromBinaryAs =<< obj .: "source"
    destination <- fromTagged fromBinaryAs =<< obj .: "destination"
    pure Mapping{..}

taggedBinaryToJSON :: Mapping -> Value
taggedBinaryToJSON x =
    object
        [ "source" .= toTagged toBinaryAs (source x)
        , "destination" .= toTagged toBinaryAs (destination x)
        ]

testTaggedBinary :: IO ()
testTaggedBinary = do
    putStrLn "## Tagged Binary"
    let
        json = taggedBinaryToJSON example
    print . encode $ json -- TODO: Use aeson-pretty
    mapping <- parseThrow taggedBinaryFromJSON json
    guard $ mapping == example
    print mapping

taggedTextualFromJSON :: Value -> Parser Mapping
taggedTextualFromJSON = withObject "Mapping" $ \obj -> do
    source <- fromTagged (fromTextAs @Unicode) =<< obj .: "source"
    destination <- fromTagged (fromTextAs @Unicode) =<< obj .: "destination"
    pure Mapping{..}

taggedTextualToJSON :: Mapping -> IO Value
taggedTextualToJSON x = do
    source' <- toTaggedM (toTextAs @Unicode) (source x)
    destination' <- toTaggedM (toTextAs @Unicode) (destination x)
    pure . object $
        [ "source" .= source'
        , "destination" .= destination'
        ]

testTaggedTextual :: IO ()
testTaggedTextual = do
    putStrLn "## Tagged Textual"
    json <- taggedTextualToJSON example
    print . encode $ json -- TODO: Use aeson-pretty
    mapping <- parseThrow taggedTextualFromJSON json
    guard $ mapping == example
    print mapping

test :: IO ()
test = do
    putStrLn "# Functions"
    testBinary
    testTextual
    testTaggedBinary
    testTaggedTextual
