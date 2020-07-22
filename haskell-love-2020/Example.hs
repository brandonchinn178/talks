{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

import Data.Aeson (eitherDecodeFileStrict)
import Data.Aeson.Schema

type MySchema = [schema|
  {
    users: List {
      id: Int,
      name: Text,
    },
  }
|]

getResult :: IO (Object MySchema)
getResult = either fail return =<< eitherDecodeFileStrict "example.json"
