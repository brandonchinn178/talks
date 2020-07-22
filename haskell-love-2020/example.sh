#!/usr/bin/env bash

exec stack ghci Example.hs \
    --package aeson \
    --package aeson-schemas \
    --ghci-options '-XQuasiQuotes -XDataKinds -XTypeApplications'
