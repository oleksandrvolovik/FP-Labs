module Lab2 where

import Prelude

import Data.List (List(..), length, reverse)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)

-- Поверне перший індекс елементу списку, для якого виконується предикат.
findIndex :: forall a. (a -> Boolean) -> List a -> Maybe Int
findIndex fn = go 0
  where
  go :: Int -> List a -> Maybe Int
  go n (Cons x xs) | fn x = Just n
                   | otherwise = go (n + 1) xs
  go _ Nil = Nothing
  
  
-- Поверне індекс останнього елементу списку, для якого виконується предикат.
findLastIndex :: forall a. (a -> Boolean) -> List a -> Maybe Int
findLastIndex fn xs = go (length(xs) - 1) (reverse(xs))
  where
    go :: Int -> List a -> Maybe Int
    go n (Cons a as) | fn a = Just n
                     | otherwise = go (n - 1) as
    go _ Nil = Nothing

-- Заархівує 2 списки в один список, де кожен елемент вихідного списку є Tuple який містить перший елемент List у першій позиції та другий List у другій позиції.
zip :: forall a b. List a -> List b -> List (Tuple a b)
zip xs ys = reverse $ go xs ys Nil
  where
  go Nil _ acc = acc
  go _ Nil acc = acc
  go (Cons a as) (Cons b bs) acc = go as bs $ Cons (Tuple a b) acc

-- unzip є зворотним до zip.
unzip :: forall a b. List (Tuple a b) -> Tuple (List a) (List b)
unzip xy = go xy Nil Nil
  where
  go Nil acc1 acc2 = Tuple (reverse acc1) (reverse acc2)
  go (Cons a as) Nil Nil = go as (Cons (fst a) Nil) (Cons (snd a) Nil)
  go (Cons a as) acc1 acc2 = go as (Cons (fst a) acc1) (Cons (snd a) acc2)

-- Поверне список з елементів, для яких виконується предикат.
filter :: forall a. (a -> Boolean) -> List a -> List a
filter _ Nil = Nil
filter p (Cons x xs) | p x = Cons x $ filter p xs
                     | otherwise = filter p xs

-- filter, оптимізований методом хвостової оптимізації
tailRecursionFilter :: forall a. (a -> Boolean) -> List a -> List a
tailRecursionFilter p = go Nil
  where
  go acc Nil = reverse acc
  go acc (Cons x xs)
    | p x = go (Cons x acc) xs
    | otherwise = go acc xs

-- Поверне вказану кількість елементів зі списку або стільки, скільки зможе, якщо список замалий.
take :: forall a. Int -> List a -> List a
take 0 _ = Nil
take _ Nil = Nil
take n (Cons x xs) = Cons x $ take (n - 1) xs

-- take, оптимізований методом хвостової оптимізації
tailRecursionTake :: forall a. Int -> List a -> List a
tailRecursionTake = go Nil
  where
  go acc 0 _ = reverse acc
  go acc _ Nil = reverse acc
  go acc n (Cons x xs) = go (Cons x acc) (n - 1) xs

test :: Effect Unit
test = do
  let x = Cons 5 (Cons 4 (Cons 3 (Cons 2 (Cons 1 Nil))))
  let y = Cons "e" (Cons "d" (Cons "c" (Cons "b" (Cons "a" Nil))))
  log $ show $ x
  log $ show $ findIndex (_ <= 3) x
  log $ show $ findLastIndex (_ <= 3) x
  let xy = zip x y
  log $ show $ xy
  log $ show $ fst $ unzip xy
  log $ show $ snd $ unzip xy
  log $ show $ filter (_ <= 3) x
  log $ show $ take 10 x
