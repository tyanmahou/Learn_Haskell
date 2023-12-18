import Data.Monoid;
import Data.Foldable qualified as F

newtype CharList = CharList {getCharList :: [Char]} deriving (Eq, Show)

newtype Pair b a = Pair {getPair :: (a, b)}

instance Functor (Pair c) where
  fmap f (Pair (x, y)) = Pair (f x, y)

newtype CoolBool = CoolBool {getCoolBool :: Bool}

hellowMe :: CoolBool -> String
hellowMe (CoolBool _) = "hello"

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

instance F.Foldable Tree where
  foldMap f EmptyTree = mempty
  foldMap f (Node x l r) =
    F.foldMap f l
      `mappend` f x
      `mappend` F.foldMap f r

testTree :: Tree Int
testTree = Node 5 ( Node 3 (Node 1 EmptyTree EmptyTree) (Node 6 EmptyTree EmptyTree) ) ( Node 9 (Node 8 EmptyTree EmptyTree) (Node 10 EmptyTree EmptyTree))