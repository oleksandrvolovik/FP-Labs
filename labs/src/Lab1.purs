module Lab1 where

import Prelude

import Data.List.Types (List(..))
import Effect (Effect)
import Effect.Console (log)

singleton :: forall a. a -> List a
singleton a = (Cons a Nil)

null :: forall a. List a -> Boolean
null Nil = true
null _ = false

snoc :: forall a. List a -> a -> List a
snoc Nil x = singleton(x)
snoc (Cons head tail) x = Cons head $ snoc tail x
 
length :: forall a. List a -> Int 
length Nil = 0
length (Cons _ tail) = 1 + length tail 

test :: Effect Unit
test = do
  let x = singleton("2")
  log $ show $ x
  log $ show $ null $ singleton("hello")
  let y = Cons "1" x
  log $ show $ snoc y "3"
  log $ show $ length $ snoc y "3"

