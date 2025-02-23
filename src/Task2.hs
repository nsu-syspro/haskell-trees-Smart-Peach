{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task2 where

-- Explicit import of Prelude to hide functions
-- that are not supposed to be used in this assignment
import Prelude hiding (compare, foldl, foldr, Ordering(..))

import Task1 (Tree(..), Order(..), torder)

-- * Type definitions

-- | Ordering enumeration
data Ordering = LT | EQ | GT
  deriving (Show, Eq)


-- | Binary comparison function indicating whether first argument is less, equal or
-- greater than the second one (returning 'LT', 'EQ' or 'GT' respectively)
type Cmp a = a -> a -> Ordering

-- * Function definitions

-- | Binary comparison function induced from `Ord` constraint
--
-- Usage example:
--
-- >>> compare 2 3
-- LT
-- >>> compare 'a' 'a'
-- EQ
-- >>> compare "Haskell" "C++"
-- GT
--
compare :: Ord a => Cmp a
compare first second
    | first < second = LT
    | first > second = GT
    | otherwise = EQ

-- | Conversion of list to binary search tree
-- using given comparison function
--
-- Usage example:
--
-- >>> listToBST compare [2,3,1]
-- Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf)
-- >>> listToBST compare ""
-- Leaf
--
listToBST :: Cmp a -> [a] -> Tree a
listToBST _ [] = Leaf
listToBST cmp (x:xs) = tinsert cmp x (listToBST cmp xs)

-- | Conversion from binary search tree to list
--
-- Resulting list will be sorted
-- if given tree is valid BST with respect
-- to some 'Cmp' comparison.
--
-- Usage example:
--
-- >>> bstToList (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- [1,2,3]
-- >>> bstToList Leaf
-- []
--
bstToList :: Tree a -> [a]
bstToList tree = torder InOrder Nothing tree 

-- | Tests whether given tree is a valid binary search tree
-- with respect to given comparison function
--
-- Usage example:
--
-- >>> isBST compare (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- True
-- >>> isBST compare (Leaf :: Tree Char)
-- True
-- >>> isBST compare (Branch 5 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- False
--
isBST :: Cmp a -> Tree a -> Bool
isBST cmp tree = isBSTWithLimits  cmp tree Nothing Nothing
-- redefine comparison operator for Nothing and au other type
isBSTWithLimits :: Cmp a -> Tree a -> Maybe a -> Maybe a -> Bool
isBSTWithLimits _ Leaf _ _ = True
isBSTWithLimits cmp (Branch root l r) Nothing Nothing = (isBSTWithLimits cmp l Nothing (Just root)) && (isBSTWithLimits cmp r (Just root) Nothing)
isBSTWithLimits cmp (Branch root l r) (Just lowerLimit) Nothing
  | cmp root lowerLimit == GT = (isBSTWithLimits cmp l (Just lowerLimit) (Just root)) && (isBSTWithLimits cmp r (Just root) Nothing)
  | otherwise = False
isBSTWithLimits cmp (Branch root l r) Nothing (Just upperLimit)
  | cmp root upperLimit == LT = (isBSTWithLimits cmp l Nothing (Just root)) && (isBSTWithLimits cmp r (Just root) (Just upperLimit))
  | otherwise = False
isBSTWithLimits cmp (Branch root l r) (Just lowerLimit) (Just upperLimit)
  | cmp root upperLimit == LT && cmp root lowerLimit == GT = (isBSTWithLimits cmp l (Just lowerLimit) (Just root)) && (isBSTWithLimits cmp r (Just root) (Just upperLimit))
  | otherwise = False
-- | Searches given binary search tree for
-- given value with respect to given comparison
--
-- Returns found value (might not be the one that was given)
-- wrapped into 'Just' if it was found and 'Nothing' otherwise.
--
-- Usage example:
--
-- >>> tlookup compare 2 (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- Just 2
-- >>> tlookup compare 'a' Leaf
-- Nothing
-- >>> tlookup (\x y -> compare (x `mod` 3) (y `mod` 3)) 5 (Branch 2 (Branch 0 Leaf Leaf) (Branch 2 Leaf Leaf))
-- Just 2
--
tlookup :: Cmp a -> a -> Tree a -> Maybe a
tlookup _ _ Leaf = Nothing
tlookup cmp val (Branch root l r)
  | cmp val root == LT = tlookup cmp val l
  | cmp val root == GT = tlookup cmp val r
  | otherwise = Just root

-- | Inserts given value into given binary search tree
-- preserving its BST properties with respect to given comparison
--
-- If the same value with respect to comparison
-- was already present in the 'Tree' then replaces it with given value.
--
-- Usage example:
--
-- >>> tinsert compare 0 (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- Branch 2 (Branch 1 (Branch 0 Leaf Leaf) Leaf) (Branch 3 Leaf Leaf)
-- >>> tinsert compare 1 (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf)
-- >>> tinsert compare 'a' Leaf
-- Branch 'a' Leaf Leaf
--
tinsert :: Cmp a -> a -> Tree a -> Tree a
tinsert _ n Leaf = (Branch n Leaf Leaf)
tinsert cmp n (Branch node l r)
  | cmp n node == LT = Branch node (tinsert cmp n l) r
  | cmp n node == GT = Branch node l (tinsert cmp n r)
  | otherwise = (Branch n l r)

-- | Deletes given value from given binary search tree
-- preserving its BST properties with respect to given comparison
--
-- Returns updated 'Tree' if the value was present in it;
-- or unchanged 'Tree' otherwise.
--
-- Usage example:
--
-- >>> tdelete compare 1 (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- Branch 2 Leaf (Branch 3 Leaf Leaf)
-- >>> tdelete compare 'a' Leaf
-- Leaf
--
tdelete :: Cmp a -> a -> Tree a -> Tree a
tdelete _ _ Leaf = Leaf
tdelete cmp n (Branch a l r)
  | cmp n a == LT = (Branch a (tdelete cmp n l) r)
  | cmp n a == GT = (Branch a l (tdelete cmp n r))
  | otherwise = deleteRoot cmp (Branch a l r)

deleteRoot :: Cmp a -> Tree a -> Tree a
deleteRoot _ Leaf = Leaf
deleteRoot _ (Branch _ Leaf Leaf) = Leaf
deleteRoot _ (Branch _ Leaf r) = r
deleteRoot _ (Branch _ l Leaf) = l
deleteRoot cmp (Branch _ l r) = (Branch (findMin r) l (tdelete cmp (findMin r) r))

findMin :: Tree a -> a
findMin (Branch a l _) = case l of
  Leaf -> a
  (Branch _ _ _) -> findMin l
findMin Leaf = error "Cannot find min in empty Tree"
