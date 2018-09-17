module Helper (
  get1st,
  get2nd,
  get3rd,
  removeAt,
  Tree,
  RAList,
  empty,
  toRAList,
  fromRAList,
  index,
  update,
  cons,
  tail,
  head
  ) where

import Prelude hiding (head, tail)
import Data.List (mapAccumL)

get1st :: (a, b, c) -> a
get1st (x,_, _) = x

get2nd :: (a, b, c) -> b
get2nd (_,x,_) = x

get3rd :: (a, b, c) -> c
get3rd (_,_,x) = x

removeAt :: Int -> [a] -> (a, [a])
removeAt k xs = case back of
  [] -> error "removeAt: index too large"
  x:rest -> (x, front ++ rest)
  where (front, back) = splitAt (k-1) xs

--Credit for the section below goes to Erlend Hamburg's implementation of the Random Access List
--provided on http://hamberg.no/erlend/posts/2012-08-29-purely-functional-random-access-list.html
data Tree a = Leaf a | Node a (Tree a) (Tree a) deriving (Show, Eq)

type RAList a = [(Int, Tree a)]

--Construct an empty Random Access List
empty :: RAList a
empty = []

--Construct a Random Access List from a normal List (O(n))
fromRAList :: [a] -> RAList a
fromRAList l = fst $ mapAccumL (\l e -> (cons e l,l)) empty (reverse l)

--Convert an RAList into a normal list (O(n))
toRAList :: RAList a -> [a]
toRAList [] = []
toRAList xs = head xs : toRAList (tail xs)

--Take an RAList and an index and return the item at that index (O(logn))
index :: RAList a -> Int -> a
index ((size, t) :r) i = if i < size
  then index' (size,t) i
  else index r (i-size)
    where index' (size, Leaf x) 0 = x
          index' (size, Leaf x) i = error "index: out of bounds"
          index' (size, Node x t1 t2) 0 = x
          index' (size, Node x t1 t2) i =
            let size' = size `div` 2
              in if i <= size'
                then index' (size', t1) (i-1)
                else index' (size', t2) (i-1-size')

--Take an RAList, an index, and a value and update the value at the given index (O(logn))
update :: RAList a -> Int -> a -> RAList a
update ((size,t):r) i e = if i < size
  then (size, update' size t i e):r
  else update r (i-size) e
    where update' size (Leaf x) 0 y = Leaf y
          update' size (Leaf _) i y = error "update: index out bounds"
          update' size (Node x t1 t2) 0 y = Node y t1 t2
          update' size (Node x t1 t2) i y =
            let size' = size `div` 2
              in if i <= size'
                then Node x (update' size' t1 (i-1) y) t2
                else Node x  t1 (update' size' t2 (i-1-size') y)

--Prepends a given value onto a RAList
cons :: a -> RAList a -> RAList a
cons x xs@((s1,t1):(s2,t2):rest) =
  if s1 == s2
    then (1+s1+s2, Node x t1 t2):rest
    else (1, Leaf x):xs
cons x xs = (1, Leaf x):xs

--Return a list's tail
tail :: RAList a ->RAList a
tail [] = error "tail: empty list"
tail ((_, Leaf _):rest) = rest
tail ((s, Node _ t1 t2):rest) = (s',t1):(s',t2):rest
  where s' = (s-1) `div` 2

--Return a list's head
head :: RAList a -> a
head [] = error "head: empty list"
head ((_, Leaf x ):_) = x
head ((_, Node x _ _):_) = x
