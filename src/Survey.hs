{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Survey (
j
) where

import Data.Aeson
import Data.Text
import Data.Monoid
import GHC.Generics

data Person = Person {
      name :: Text
    , age  :: Int
    } deriving Show

instance ToJSON Person where
    -- this generates a Value
    toJSON (Person name age) =
        object ["name" .= name, "age" .= age]

    -- this encodes directly to a bytestring Builder
    toEncoding (Person name age) =
        pairs ("name" .= name <> "age" .= age)


-- j = encode (Person {name = "Joe", age = 12}) 
j = encode $ object ["name" .= ("fa"::Text)]


