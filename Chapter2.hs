module Chapter2 where

import Control.Monad (liftM)
import Control.Monad.Instances

{- 
==========================
2.1 Lists 
==========================
-}

-- // --------------------------
-- // Exercise 2.1
-- // --------------------------
suffixes [] = [[]]
suffixes xs = xs : (suffixes $ tail xs)
-- \\ --------------------------

{- 
==========================
2.2 Binary Search Trees 
==========================
-}

data BSTree a = Empty | Node (BSTree a) a (BSTree a) 
                       deriving (Show, Eq)

mkTree = 
  (Node 
   (Node (Node Empty 1 Empty) 
    3 (Node (Node Empty 4 Empty) 6 (Node Empty 7 Empty)))
   8
   (Node Empty 10 (Node (Node Empty 13 Empty) 14 Empty)))

member :: (Ord a) => a -> BSTree a -> Bool
member _ Empty = False
member x (Node l v r)
  | x < v = member x l 
  | x > v = member x r
  | otherwise = True
                
insert :: (Ord a) => a -> BSTree a -> BSTree a
insert x Empty = Node Empty x Empty
insert x (Node l v r)
  | x < v = Node (insert x l) v r
  | v < x = Node l v (insert x r)
  | otherwise = (Node l x r)

-- // --------------------------
-- // Exercise 2.2
-- // --------------------------
member2 :: (Ord a) => a -> BSTree a -> Bool
member2 _ Empty = False
member2 x t@(Node l v r) = member' t v
  where member' Empty c = x == c
        member' (Node a y b) c = 
          if x < y then
            member' a c
          else member' b y
-- \\ --------------------------

-- // --------------------------
-- // Exercise 2.3
-- // --------------------------
insert2 :: (Ord a) => a -> BSTree a -> Either String (BSTree a)
insert2 x Empty = return (Node Empty x Empty)
insert2 x (Node l v r)
  | x < v = liftM (\t -> Node t v r) (insert2 x l)
  | v < x = liftM (\t -> Node l v t) (insert2 x r)
  | otherwise = fail "insert2: element already exists"
-- \\ --------------------------

-- // --------------------------
-- // Exercise 2.4
-- // --------------------------
insert3 :: (Ord a) => a -> BSTree a -> Either String (BSTree a)
insert3 x Empty = return (Node Empty x Empty)
insert3 x s@(Node _ v _) = insert' s v
  where insert' Empty c = 
          if x == c then
            fail "insert3: element already exists"
          else insert3 x Empty
        insert' (Node a y b) c =
          if x < y then
            liftM (\t -> Node t y b) (insert' a c)
          else liftM (\t -> Node a y t) (insert' b y)
-- \\ --------------------------

-- // --------------------------
-- // Exercise 2.5
-- // --------------------------
complete :: a -> Int -> BSTree a
complete x d
  | d == 0 = Node Empty x Empty
  | d > 0 = let stree = complete x (d - 1)
              in Node stree x stree
  | otherwise = Empty
                
balanced :: a -> Int -> BSTree a
balanced x n -- D&C with even/odd division
  | n <= 0 = Empty
  | n == 1 = Node Empty x Empty
  | even (n - 1) = 
      let stree = balanced x (div2 $ n - 1)
        in Node stree x stree
  | otherwise = let (ltree, rtree) = balanced2 (div2 $ n - 1)
                    in Node ltree x rtree
  where balanced2 m = (balanced x m, balanced x (m + 1))
        div2 = (`div` 2)
-- \\ --------------------------

-- // --------------------------
-- // Exercise 2.6
-- // --------------------------
type Map k v = BSTree (Key k, Val v)
newtype Key k = Key k deriving (Show, Eq)
newtype Val v = Val v deriving (Show)

instance (Ord k) => Ord (Key k) where   
  (Key k) `compare` (Key k') = compare k k'
instance Eq (Val v) where 
  _ == _ = True
instance Ord (Val v) where 
  _ `compare` _ = EQ

emptyM = Empty

bindM :: (Ord k, Ord v) => Key k -> Val v -> Map k v -> Map k v
bindM k v m = insert (k, v) m

lookupM :: (Ord k, Ord v) => Key k -> Map k v -> Either String (Val v)
lookupM _ Empty = fail "lookupM: element does not exist"
lookupM k (Node a (k', v) b)
  | k < k' = lookupM k a
  | k > k' = lookupM k b
  | otherwise = return v
-- \\ --------------------------
