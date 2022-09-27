module Lab1 where

import Prelude

import Data.List.Types (List(..), (:))
import Effect (Effect)
import Effect.Console (log)

singleton :: forall a. a -> List a
singleton a = (Cons a Nil)

null :: forall a. List a -> Boolean
null Nil = true
null _ = false

snoc :: forall a. List a -> a -> List a
snoc xs x = reverse $ Cons x $ reverse xs

length :: forall a. List a -> Int 
length a = length' a 0

length' :: forall a. List a -> Int -> Int
length' Nil totalCount = totalCount
length' (Cons head tail) currentCount = length' tail (currentCount  + 1)

reverse :: List ~> List
reverse = go Nil
  where
  go acc Nil = acc
  go acc (x : xs) = go (x : acc) xs


test :: Effect Unit
test = do
  let x = singleton("2")
  log $ show $ x
  log $ show $ null $ singleton("hello")
  let y = Cons "1" x
  log $ show $ snoc y "3"
  log $ show $ length $ snoc y "3"

