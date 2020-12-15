data Tree a = Node (Tree a) a Int (Tree a) | Nil deriving Show


leaf :: a -> Tree a
leaf key = Node Nil key 1 Nil


key :: Tree a -> a
key (Node _ key _ _) = key


depth :: Tree a -> Int
depth (Node _ _ d _) = d
depth Nil = 0


after :: Ord a => Tree a -> Tree a
after node@(Node Nil _ _ _) = node
after (Node left _ _ _) = after left


insert :: Ord a => Tree a -> Tree a -> Tree a
insert node Nil = node

insert node tree@(Node left k d right)
  | key node < k = balance (update (Node (insert node left) k d right))
  | key node > k = balance (update (Node left k d (insert node right)))
  | otherwise = tree


delete :: Ord a => a -> Tree a -> Tree a
delete key Nil = Nil

delete key tree@(Node left k d right)
  | key < k = balance (update (Node (delete key left) k d right))
  | key > k = balance (update (Node left k d (delete key right)))
  | otherwise = delete' tree


delete' :: Ord a => Tree a -> Tree a
delete' (Node Nil k d Nil) = Nil
delete' (Node left k d Nil) = left
delete' (Node Nil k d right) = right


delete' node@(Node left k d right)
  = update (Node left k' d (delete k' right))
    where k' = key (after right)


balance :: Tree a -> Tree a

balance node@(Node left key d right)
  | wn == 2 && wl >= 0 = rotateR node
  | wn == -2 && wr <= 0 = rotateL node
  | wn == 2 && wl == -1 = rotateR (Node left key d (rotateL left))
  | wn == -2 && wr == 1 = rotateL (Node (rotateR right) key d left)
  | otherwise = node
  where wn = weight node
        wl = weight left
        wr = weight right


weight :: Tree a -> Int
weight (Node left _ _ right) = depth left - depth right


rotateR :: Tree a -> Tree a
rotateR (Node (Node l k d' r) key d right)
  = update (Node l k d' (update (Node r key d right)))


rotateL :: Tree a -> Tree a
rotateL (Node left key d (Node l k d' r))
  = update (Node (update (Node left key d l)) k d' r)


update :: Tree a -> Tree a
update (Node left key d right) = Node left key d' right
  where d' = 1 + max (depth right) (depth left)


-- interface

u [] = Nil
u (key : keys) = insert (leaf key) (u keys)


fromList :: Ord a => [a] -> Tree a
fromList keys = u (reverse keys) 


toList :: Ord a => Tree a -> [a]
toList Nil = []
toList (Node left key _ right) 
  = (toList left) ++ [key] ++ (toList right)
