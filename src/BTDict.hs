module BTDict (
  BTDict,
  emptyBT,
  insertBT,
  deleteBT,
  lookupBT,
  mapValuesBT,
  filterBT,
  foldlBT,
  foldrBT,
) where

data BTDict k v
  = Empty
  | Node (BTDict k v) (k, v) (BTDict k v)
  deriving (Show)

emptyBT :: BTDict k v
emptyBT = Empty

findMinBT :: BTDict k v -> Maybe (k, v)
findMinBT (Node Empty kv _) = Just kv
findMinBT (Node left _ _) = findMinBT left
findMinBT Empty = Nothing

mergeBT :: (Ord k) => BTDict k v -> BTDict k v -> BTDict k v
mergeBT Empty r = r
mergeBT l Empty = l
mergeBT l r =
  case findMinBT r of
    Just (kMin, vMin) ->
      Node l (kMin, vMin) (deleteBT kMin r)
    Nothing -> l

insertBT :: (Ord k) => k -> v -> BTDict k v -> BTDict k v
insertBT k v Empty = Node Empty (k, v) Empty
insertBT k v (Node left (k', v') right)
  | k < k' = Node (insertBT k v left) (k', v') right
  | k > k' = Node left (k', v') (insertBT k v right)
  | otherwise = Node left (k, v) right

deleteBT :: (Ord k) => k -> BTDict k v -> BTDict k v
deleteBT _ Empty = Empty
deleteBT k (Node left (k', v') right)
  | k < k' = Node (deleteBT k left) (k', v') right
  | k > k' = Node left (k', v') (deleteBT k right)
  | otherwise =
      case (left, right) of
        (Empty, Empty) -> Empty
        (Empty, _) -> right
        (_, Empty) -> left
        _ -> case findMinBT right of
          Just (kMin, vMin) ->
            Node left (kMin, vMin) (deleteBT kMin right)
          Nothing -> left

lookupBT :: (Ord k) => k -> BTDict k v -> Maybe v
lookupBT _ Empty = Nothing
lookupBT k (Node left (k', v') right)
  | k < k' = lookupBT k left
  | k > k' = lookupBT k right
  | otherwise = Just v'

mapValuesBT :: (v -> v2) -> BTDict k v -> BTDict k v2
mapValuesBT _ Empty = Empty
mapValuesBT f (Node left (k', v') right) =
  Node (mapValuesBT f left) (k', f v') (mapValuesBT f right)

filterBT :: (Ord k) => ((k, v) -> Bool) -> BTDict k v -> BTDict k v
filterBT _ Empty = Empty
filterBT f (Node left (k', v') right)
  | f (k', v') =
      Node (filterBT f left) (k', v') (filterBT f right)
  | otherwise =
      mergeBT (filterBT f left) (filterBT f right)

foldlBT :: (a -> (k, v) -> a) -> a -> BTDict k v -> a
foldlBT _ acc Empty = acc
foldlBT f acc (Node left kv right) =
  let accLeft = foldlBT f acc left
      accNode = f accLeft kv
   in foldlBT f accNode right

foldrBT :: ((k, v) -> a -> a) -> a -> BTDict k v -> a
foldrBT _ acc Empty = acc
foldrBT f acc (Node left kv right) =
  let accRight = foldrBT f acc right
      accNode = f kv accRight
   in foldrBT f accNode left

-- TODO: FIX, SO TREES WITH DIFFERENT SHAPES, BUT WITH EQUAL PAIR SETS ARE EQUAL
eqBT :: (Eq k, Eq v) => BTDict k v -> BTDict k v -> Bool
eqBT Empty Empty = True
eqBT Empty _ = False
eqBT _ Empty = False
eqBT (Node left1 (k1, v1) right1) (Node left2 (k2, v2) right2)
  | (k1 == k2) && (v1 == v2) = eqBT left1 left2 && eqBT right1 right2
  | otherwise = False

instance (Ord k) => Semigroup (BTDict k v) where
  dict1 <> dict2 = foldlBT (\acc (k, v) -> insertBT k v acc) dict1 dict2

instance (Ord k) => Monoid (BTDict k v) where
  mempty = emptyBT

instance (Ord k, Eq v) => Eq (BTDict k v) where
  (==) = eqBT
