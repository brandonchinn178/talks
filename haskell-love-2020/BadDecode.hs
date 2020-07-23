{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Data.Aeson (eitherDecode)
import Data.Aeson.Schema (Object, schema)

main :: IO ()
main = do
  o <- either fail return $ eitherDecode "{ \"foo\": { \"bar\": 1 } }"
  print (o :: Object [schema| { foo: { bar: List Text } } |])
