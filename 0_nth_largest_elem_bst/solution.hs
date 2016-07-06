{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
import Data.Ratio
import Control.Lens hiding (Empty)
import Data.Functor.Reverse
import Control.Monad

data BT a = Empty | Bin a (BT a) (BT a)
  deriving (Show)

insert :: Ord a => a -> BT a -> BT a
insert x Empty = Bin x Empty Empty
insert x (Bin y left right)
  | x < y = Bin y (insert x left) right
  | otherwise = Bin y left $ insert x right

fromList :: Ord a => [a] -> BT a
fromList = foldl (\m x -> insert x m) Empty

----------------------------------------------

-- A very naive approach: flatten to a list and then take the nth item safely
-- Due to the reverse, it isn't very lazy

flatten :: BT a -> [a]
flatten Empty = []
flatten (Bin x left right) = flatten left ++ [x] ++ flatten right

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

nthLargest :: Int -> BT a -> Maybe a
nthLargest n t = safeHead $ drop n $ reverse $ flatten t

----------------------------------------------

flattenReversed :: BT a -> [a]
flattenReversed Empty = []
flattenReversed (Bin x left right) = flattenReversed right ++ [x] ++ flattenReversed left

nthLargest' :: Int -> BT a -> Maybe a
nthLargest' n t = safeHead $ drop n $ flattenReversed t

----------------------------------------------

-- A recursive solution we might expect to see in an interview

-- Either we've found the element (Left) or we've seen a greater elements (Right a)
nthLargestHelper :: Int -> BT a -> Either a Int
nthLargestHelper n Empty = Right 0
nthLargestHelper n (Bin x left right) = case nthLargestHelper n right of
  Left a -> Left a
  Right nrViewed -> if nrViewed == n
    then Left x
    else case nthLargestHelper (n - nrViewed - 1) left of
      Left a -> Left a
      Right nlViewed -> Right (nrViewed + nlViewed + 1)

-- this is also called leftToMaybe, I think
safeLeft :: Either a b -> Maybe a
safeLeft (Left a) = Just a
safeLeft _ = Nothing

nthLargest'' :: Int -> BT a -> Maybe a
nthLargest'' n t = safeLeft $ nthLargestHelper n t

-----------------------------------------------

-- The same solution using do-notation

nthLargestHelper' :: Int -> BT a -> Either a Int
nthLargestHelper' n Empty = Right 0
nthLargestHelper' n (Bin x left right) = do
  nrViewed <- nthLargestHelper' n right
  when (nrViewed == n) (Left x)
  nlViewed <- nthLargestHelper' (n - nrViewed - 1) left
  return (nrViewed + nlViewed + 1)

nthLargest''' :: Int -> BT a -> Maybe a
nthLargest''' n t = safeLeft $ nthLargestHelper n t

-----------------------------------------------

-- A dark magic solution using lenses

nthLargest'''' :: Int -> BT a -> Maybe a
nthLargest'''' n t = Reverse t ^? element n

-----------------------------------------------

instance Functor BT where
  fmap _ Empty = Empty
  fmap f (Bin x left right) = Bin (f x) (fmap f left) (fmap f right)

instance Foldable BT where
  -- foldMap :: Monoid m => (a -> m b) -> BT a -> m b
  foldMap _ Empty = mempty
  foldMap f (Bin x left right) = foldMap f left `mappend` f x `mappend` foldMap f right

instance Traversable BT where
  -- traverse :: Applicative f => (a -> f b) -> BT a -> f (BT a)
  traverse f Empty = pure Empty
  traverse f (Bin x left right) = flip Bin <$> (traverse f left) <*> (f x) <*> (traverse f right)

-----------------------------------------------

-- Misc

preorder :: BT a -> [a]
preorder (Bin x left right) = [x] ++ preorder left ++ preorder right

breadthFlatten :: BT a -> [a]
breadthFlatten Empty = []
breadthFlatten t = concat $ breadthFlattenHelper t

zipWith' :: (a -> b -> c) -> a -> b -> [a] -> [b] -> [c]
zipWith' z _ _ [] [] = []
zipWith' z da db as [] = zipWith' z da db as [db]
zipWith' z da db [] bs = zipWith' z da db [da] bs
zipWith' z da db (a:as) (b:bs) = z a b : zipWith' z da db as bs

breadthFlattenHelper :: BT a -> [[a]]
breadthFlattenHelper Empty = []
breadthFlattenHelper (Bin x left right) = [x] : zipWith' (++) [] [] (breadthFlattenHelper left) (breadthFlattenHelper right)

----------------------------------------------

interleave [] _ = []
interleave _ [] = []
interleave (x:xs) y = x:(interleave y xs)

smalls = scanl (\m x -> m + (1 % 2) ^^ x) 0 [2..]
larges = scanl (\m x -> m - (1 % 2) ^^ x) 1 [2..]
tighteningSet :: [Rational]
tighteningSet = interleave smalls larges

-----------------------------------------------

zigZagTree :: BT Rational
zigZagTree = Bin 0 Empty (Bin 1 (fmap (\x -> (x + 1 % 2) / 2) zigZagTree) Empty)
