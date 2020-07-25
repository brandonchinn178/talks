{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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

class StatusInfo (status :: Status) where
  statusLabel :: String

instance StatusInfo 'OPEN where
  statusLabel = "open"

instance StatusInfo 'CLOSED where
  statusLabel = "closed"

showRestaurantMessage :: forall status. StatusInfo status => Restaurant status -> String
showRestaurantMessage restaurant =
  "Restaurant " ++ getName restaurant ++ " is " ++ statusLabel @status
