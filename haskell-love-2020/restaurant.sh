#!/usr/bin/env bash

builtin cd "$(dirname "${BASH_SOURCE[0]}")"

exec stack ghci Restaurant.hs \
    --ghci-options '-XDataKinds'

# DEMO:
#   >>> :k Restaurant
#   >>> :k Restaurant 'OPEN
#   >>> let restaurant = createRestaurant "McDonalds"
#   >>> :t restaurant
#   >>> openRestaurant restaurant
#   >>> showRestaurantMessage restaurant
#   >>> showRestaurantMessage $ closeRestaurant restaurant
