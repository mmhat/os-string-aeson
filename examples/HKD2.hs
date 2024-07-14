{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module HKD2 (test) where

import Control.Monad (guard)
import Data.Aeson.Types (FromJSON (..), ToJSON (..))
import Data.Kind (Type)
import GHC.Generics (Generic)
import System.OsString
import System.OsString.Aeson

import Utils

data RepTag = Id | JSON

type family FieldRep (t :: RepTag) (a :: Type) :: Type where
    FieldRep 'Id a = a
    FieldRep 'JSON OsString = As ('Text Unicode :: Tag 'TopLevel) OsString
    FieldRep 'JSON a = a

type Mapping = Mapping' 'Id

data Mapping' t = Mapping
    { source :: FieldRep t OsString
    , destination :: FieldRep t OsString
    }
    deriving (Generic)

deriving instance Eq (Mapping' 'Id)

deriving instance Eq (Mapping' 'JSON)

instance FromJSON (Mapping' 'JSON)

instance ToJSON (Mapping' 'JSON)

deriving via (CoercibleRep (Mapping' 'JSON) Mapping) instance FromJSON Mapping

deriving via (CoercibleRep (Mapping' 'JSON) Mapping) instance ToJSON Mapping

example :: Mapping
example =
    Mapping
        { source = exampleSource
        , destination = exampleDestination
        }

test :: IO ()
test = do
    putStrLn "# HKD 2"
    let
        json = toJSON example
    printJSON json
    mapping <- parseThrow parseJSON json
    guard $ mapping == example
