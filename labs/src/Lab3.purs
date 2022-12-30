module Lab3 where

import Data.Eq (class Eq)
import Data.Ord (class Ord, Ordering(..), compare)
import Data.Show (class Show)
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, discard, show, ($), (<), (<=), (==), (>), (>=), (<>))

data Maybe a = Nothing | Just a

instance eqMaybe :: Eq a => Eq (Maybe a) where
  eq Nothing Nothing = true
  eq (Just x) (Just y) = x == y
  eq _ _ = false

instance ordMaybe :: Ord a => Ord (Maybe a) where
  compare (Just x) (Just y) = compare x y
  compare _ _ = EQ

instance showMaybe :: Show a => Show (Maybe a) where
  show Nothing = "Nothing"
  show (Just x) = "(Just " <> show x <> ")"

test :: Effect Unit
test = do
  log $ show $ Just 5 == Just 5
  log $ show $ Just 5 == Just 2
  log $ show $ Just 5 == Nothing 
  log $ show $ Nothing == Just 5 
  log $ show $ Nothing == (Nothing :: Maybe Unit)
  log "------------------"
  log $ show $ Just 1 < Just 5
  log $ show $ Just 5 <= Just 5
  log $ show $ Just 5 > Just 10
  log $ show $ Just 10 >= Just 10
  log $ show $ Just 99 > Nothing
  log $ show $ Just 99 < Nothing
  log "------------------"
  log $ show $ Just "abc"
  log $ show $ (Nothing :: Maybe Unit)
  