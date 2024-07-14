{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Functions (test) where

import Control.Monad (guard)
import Data.Aeson.Types (Parser, Value, object, withObject, (.:), (.=))
import System.OsString
import System.OsString.Aeson

import Utils

data Mapping = Mapping
    { source :: OsString
    , destination :: OsString
    }
    deriving (Eq)

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
binaryToJSON Mapping{..} =
    object
        [ "source" .= toBinary source
        , "destination" .= toBinary destination
        ]

testBinary :: IO ()
testBinary = do
    putStrLn "## Binary"
    let
        json = binaryToJSON example
    printJSON json
    mapping <- parseThrow binaryFromJSON json
    guard $ mapping == example

textualFromJSON :: Value -> Parser Mapping
textualFromJSON = withObject "Mapping" $ \obj -> do
    source <- fromText @Unicode =<< obj .: "source"
    destination <- fromText @Unicode =<< obj .: "destination"
    pure Mapping{..}

textualToJSON :: Mapping -> IO Value
textualToJSON Mapping{..} = do
    source' <- toText @Unicode source
    destination' <- toText @Unicode destination
    pure . object $
        [ "source" .= source'
        , "destination" .= destination'
        ]

testTextual :: IO ()
testTextual = do
    putStrLn "## Textual"
    json <- textualToJSON example
    printJSON json
    mapping <- parseThrow textualFromJSON json
    guard $ mapping == example

taggedBinaryFromJSON :: Value -> Parser Mapping
taggedBinaryFromJSON = withObject "Mapping" $ \obj -> do
    source <- fromTagged fromBinaryAs =<< obj .: "source"
    destination <- fromTagged fromBinaryAs =<< obj .: "destination"
    pure Mapping{..}

taggedBinaryToJSON :: Mapping -> Value
taggedBinaryToJSON Mapping{..} =
    object
        [ "source" .= toTagged toBinaryAs source
        , "destination" .= toTagged toBinaryAs destination
        ]

testTaggedBinary :: IO ()
testTaggedBinary = do
    putStrLn "## Tagged Binary"
    let
        json = taggedBinaryToJSON example
    printJSON json
    mapping <- parseThrow taggedBinaryFromJSON json
    guard $ mapping == example

taggedTextualFromJSON :: Value -> Parser Mapping
taggedTextualFromJSON = withObject "Mapping" $ \obj -> do
    source <- fromTagged (fromTextAs @Unicode) =<< obj .: "source"
    destination <- fromTagged (fromTextAs @Unicode) =<< obj .: "destination"
    pure Mapping{..}

taggedTextualToJSON :: Mapping -> IO Value
taggedTextualToJSON Mapping{..} = do
    source' <- toTaggedM (toTextAs @Unicode) source
    destination' <- toTaggedM (toTextAs @Unicode) destination
    pure . object $
        [ "source" .= source'
        , "destination" .= destination'
        ]

testTaggedTextual :: IO ()
testTaggedTextual = do
    putStrLn "## Tagged Textual"
    json <- taggedTextualToJSON example
    printJSON json
    mapping <- parseThrow taggedTextualFromJSON json
    guard $ mapping == example

test :: IO ()
test = do
    putStrLn "# Functions"
    testBinary
    testTextual
    testTaggedBinary
    testTaggedTextual
