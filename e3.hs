-- sets using red-black trees


data Set a = Node a (Set a) (Set a) Int | Tip
  deriving Show


instance (Ord a) => Eq (Set a) where
  set1 == set2 = null' (difference set1 set2)


instance (Ord a) => Ord (Set a) where
  node1 < node2 = keyOf node1 < keyOf node2
  node1 > node2 = keyOf node1 > keyOf node2
  node1 <= node2 = not (node1 > node2)
  node1 >= node2 = not (node1 < node2)


leaf :: a -> Set a
leaf key = Node key Tip Tip 1


keyOf :: Set a -> a
keyOf (Node key _ _ _) = key


colorOf :: Set a -> Int
colorOf (Node _ _ _ red) = red


colorFlip :: Set a -> Set a
colorFlip (Node key left right red) = Node key left right (if red == 1 then 0 else 1)


rotateR :: Set a -> Set a

rotateR node@(Node key left@(Node k l r rd) right red)
  = let node' = Node key r right red
    in Node k l node' rd


rotateL :: Set a -> Set a

rotateL node@(Node key left right@(Node k l r rd) red)
  = let node' = Node key left l red
    in Node k node' r rd


insert' :: Ord a => Set a -> Set a -> Set a
insert' node set = insert (keyOf node) set


insert :: Ord a => a -> Set a -> Set a
insert key Tip = leaf key

insert key set@(Node k left right red)
  | key < k = Node k (insert key left) right red
  | key > k = Node k left (insert key right) red
  | otherwise = set











-- set operations and abstractions

fromList :: Ord a => [a] -> Set a
fromList [] = Tip
fromList (k : keys) = foldr insert' (leaf k) (map leaf keys)


toList :: Ord a => Set a -> [a]
toList Tip = []
toList (Node key left right _) = (toList left) ++ [key] ++ (toList right)


size :: Num s => Set a -> s
size Tip = 0
size (Node _ left right _) = 1 + size left + size right


empty :: Set a
empty = Tip


null' :: Set a -> Bool
null' Tip = True
null' set = False


search :: Ord a => a -> Set a -> Set a
search key Tip = Tip

search key set@(Node k left right _)
  | key < k = search key left
  | key > k = search key right
  | otherwise = set


member :: Ord a => a -> Set a -> Bool
member key set = not (null' (search key set))


union :: Ord a => Set a -> Set a -> Set a
union set Tip = set

union set (Node key left right _) 
  = let set' = insert key set 
    in union (union set' left) right


{- redo these once remove is implemented

intersection :: Ord a => Set a -> Set a -> Set a
intersection set1 set2 = fromList [key | key <- (toList set1), member key set2]

-}

difference :: Ord a => Set a -> Set a -> Set a
difference set1 set2 = fromList [key | key <- (toList set1), not (member key set2)]



setmap :: Ord b => (a -> b) -> Set a -> Set b
setmap fun Tip = Tip

setmap fun (Node key left right red)
  = Node (fun key) (setmap fun left) (setmap fun right) red


setfoldl :: (t -> a -> t) -> t -> Set a -> t
setfoldl fun acc Tip = acc

setfoldl fun acc (Node key left right _)
  = let acc' = fun (setfoldl fun acc left) key
    in setfoldl fun acc' right


setfoldr :: (a -> t -> t) -> t -> Set a -> t
setfoldr fun acc set = setfoldl (\rec x y -> rec (fun x y)) id set acc

