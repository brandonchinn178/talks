{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

data Status = OPEN | CLOSED

data Restaurant (status :: Status) = Restaurant String
  deriving (Show)

createRestaurant :: String -> Restaurant 'OPEN
createRestaurant name = Restaurant name

getName :: Restaurant status -> String
getName (Restaurant name) = name

closeRestaurant :: Restaurant 'OPEN -> Restaurant 'CLOSED
closeRestaurant (Restaurant name) = Restaurant name

openRestaurant :: Restaurant 'CLOSED -> Restaurant 'OPEN
openRestaurant (Restaurant name) = Restaurant name
