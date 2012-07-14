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

member :: Ord a => a -> BSTree a -> Bool
member _ Empty = False
member x (Node l v r)
  | x < v = member x l 
  | x > v = member x r
  | otherwise = True
                
-- // --------------------------
-- // Exercise 2.2
-- // --------------------------
member2 :: Ord a => a -> BSTree a -> Bool
member2 _ Empty = False
member2 x t@(Node l v r) = member' t v
  where member' Empty c = x == c
        member' (Node a y b) c = 
          if x < y then
            member' a c
          else member' b y
-- \\ --------------------------
