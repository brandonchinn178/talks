#!/usr/bin/env bash

builtin cd "$(dirname "${BASH_SOURCE[0]}")"

exec stack ghci Example.hs \
    --package aeson \
    --package aeson-schemas \
    --ghci-options '-XQuasiQuotes -XDataKinds -XTypeApplications'

# DEMO:
#   >>> putStrLn $ showSchema @MySchema
#   >>> result <- getResult
#   >>> result
#   >>> :t result
#   >>> [get| result.users |]
#   >>> [get| result.users[].id |]
#   >>> :t [get| result.users[].id |]
#   >>> [get| result.users[].name |]
#   >>> [get| result.users[].foo |]
