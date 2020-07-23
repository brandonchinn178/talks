#!/usr/bin/env bash

builtin cd "$(dirname "${BASH_SOURCE[0]}")"

exec stack ghci Restaurant.hs \
    --ghci-options '-XDataKinds'

# DEMO:
#   >>> let restaurant = createRestaurant "McDonalds"
#   >>> restaurant
#   >>> :t restaurant
#   >>> :k Restaurant
#   >>> :k Restaurant 'OPEN
#   >>> :t closeRestaurant restaurant
#   >>> :t openRestaurant restaurant
