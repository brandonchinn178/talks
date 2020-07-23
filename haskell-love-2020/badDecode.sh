#!/usr/bin/env bash

builtin cd "$(dirname "${BASH_SOURCE[0]}")"

exec stack runghc BadDecode.hs \
    --package aeson \
    --package aeson-schemas
