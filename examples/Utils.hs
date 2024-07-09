{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

module Utils where

import Data.Aeson.Types (Parser, Value, parseEither)
import System.OsString

exampleSource, exampleDestination :: OsString
#if defined(mingw32_HOST_OS)
exampleSource = [osstr|C:\some\directory\source|]
exampleDestination = [osstr|C:\other\directory\destination|]
#else
exampleSource = [osstr|/some/directory/source|]
exampleDestination = [osstr|/other/directory/destination|]
#endif

parseThrow :: (Value -> Parser a) -> Value -> IO a
parseThrow parse value = case parseEither parse value of
    Left e -> fail e
    Right result -> pure result
