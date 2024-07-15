{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Utils where

import Data.Aeson.Encode.Pretty (
    Config (confCompare),
    defConfig,
    encodePretty',
    keyOrder,
 )
import Data.Aeson.Types (FromJSON (..), Parser, ToJSON (..), Value, parseEither)
import Data.ByteString.Lazy qualified as ByteString
import Data.Coerce (Coercible, coerce)
import GHC.Generics (Generic, Rep, from, to)
import System.OsString

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

printJSON :: (ToJSON a) => a -> IO ()
printJSON x =
    ByteString.putStr
        (encodePretty' config x <> ByteString.singleton 10)
    where
        config :: Config
        config =
            defConfig
                { confCompare =
                    keyOrder
                        [ "platform"
                        , "data"
                        ]
                }

-- The following functions and types are necessary because of shortcomings of
-- the coercion mechanism implemented in GHC. See for examples the following
-- issues:
--      - https://gitlab.haskell.org/ghc/ghc/-/issues/14317
--      - https://gitlab.haskell.org/ghc/ghc/-/issues/15683
--      - https://gitlab.haskell.org/ghc/ghc/-/issues/22644

-- | Coerce an value of type @a@ to an value of type @b@ using its generic
-- representation.
-- The implementation is derived from the one found in the paper
-- "Deriving via: or, how to turn hand-written instances into an anti-pattern",
-- Section 4.3.
coerceViaRep
    :: forall a b
     . (Generic a, Generic b, Coercible (Rep a ()) (Rep b ()))
    => a
    -> b
coerceViaRep = to . (coerce :: Rep a () -> Rep b ()) . from

type role CoercibleRep phantom representational

-- | A newtype wrapper that suitable for the use with @DerivingVia@.
newtype CoercibleRep a b = CoercibleRep {unCoercibleRep :: b}

instance
    (Generic a, Generic b, Coercible (Rep a ()) (Rep b ()), FromJSON a)
    => FromJSON (CoercibleRep a b)
    where
    parseJSON value =
        CoercibleRep . coerceViaRep @a @_ <$> parseJSON value

instance
    (Generic a, Generic b, Coercible (Rep a ()) (Rep b ()), ToJSON a)
    => ToJSON (CoercibleRep a b)
    where
    toJSON = toJSON . coerceViaRep @_ @a . unCoercibleRep
