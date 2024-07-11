{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Utils where

import Data.Aeson.Types (ToJSON, Parser, Value, parseEither)
import System.OsString
import Data.ByteString.Lazy qualified as ByteString
import Data.Aeson.Encode.Pretty (encodePretty', defConfig, Config(confCompare)
    , keyOrder)

exampleSource, exampleDestination :: OsString
#if defined(mingw32_HOST_OS)
exampleSource = [osstr|C:\this\source|]
exampleDestination = [osstr|C:\that|]
#else
exampleSource = [osstr|/this|]
exampleDestination = [osstr|/that|]
#endif

parseThrow :: (Value -> Parser a) -> Value -> IO a
parseThrow parse value = case parseEither parse value of
    Left e -> fail e
    Right result -> pure result

printJSON :: ToJSON a => a -> IO ()
printJSON x = ByteString.putStr
    (encodePretty' config x <> ByteString.singleton 10)
    where
        config :: Config
        config = defConfig
            { confCompare = keyOrder
                [ "platform"
                , "encoding"
                , "payload"
                ]
            }
