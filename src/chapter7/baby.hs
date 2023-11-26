import qualified Data.Map as Map

data Person = Person
  { firstName :: String,
    lastName :: String,
    age :: Int
  }
  deriving (Eq, Show, Read)

mikeD = Person {firstName = "Michael", lastName ="Diamond", age = 43}
adRock = Person {firstName = "Adam", lastName ="Horovitz", age = 41}
mca = Person {firstName = "Adam", lastName ="Yauch", age = 44}

mysteryDude = "Person {firstName = \"Michael\", lastName = \"Diamond\", age = 43}"

data Car = Car
  { company :: String,
    model :: String,
    year :: Int
  }
  deriving (Show)

data Vector a = Vector a a a deriving (Show)

vplus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `vplus` (Vector l m n) = Vector (i + l) (j + m) (k + n)

dot :: (Num a) => Vector a -> Vector a -> a
(Vector i j k) `dot` (Vector l m n) = i * l + j * m + k * n

vmul :: (Num a) => Vector a -> a -> Vector a
(Vector i j k) `vmul` m = Vector (i * m) (j * m) (k * m)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturaday | Sunday
  deriving(Eq, Ord, Show, Read, Bounded, Enum)

type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]

phoneBook :: PhoneBook
phoneBook = [
 ("betty", "555-2938"),
 ("bonnie", "452-2928"),
 ("patsy", "493-2928"),
 ("lucille", "205-2928"),
 ("wendy", "939-8282"),
 ("pennny", "853-2492")
 ]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook

type AssocList k v = [(k, v)]
type IntMap = Map.Map Int

data LockerState = Taken | Free deriving(Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLockup :: Int -> LockerMap -> Either String Code
lockerLockup lockerNumber map = case Map.lookup lockerNumber map of 
  Nothing -> Left $ show lockerNumber ++ "is not exist"
  Just (state, code) -> if state /= Taken then Right code
                                          else Left $ show lockerNumber ++ "is already taken!"

lockers :: LockerMap
lockers = Map.fromList [
  (100, (Taken, "ZD39I")),
  (101, (Free, "JAH3I"))
 ]                                           

infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

infixr 5 ^++
(^++) :: List a -> List a -> List a
Empty ^++ ys = ys
(x :-: xs) ^++ ys = x :-: (xs ^++ ys)

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving(Show)
instance Functor Tree where
  fmap f EmptyTree = EmptyTree
  fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)
  
singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
 | x == a = Node x left right
 | x < a = Node a (treeInsert x left) right
 | x > a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a ->  Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
 | x == a = True
 | x < a = treeElem x left
 | x > a = treeElem x right

data TrafficLight = Red | Yellow | Green
instance Eq TrafficLight where
  (==) :: TrafficLight -> TrafficLight -> Bool
  Red == Red = True
  Yellow == Yellow = True
  Green == Green = True
  _ == _ = False

instance Show TrafficLight where
  show :: TrafficLight -> String
  show Red = "Red light"
  show Yellow = "Yellow light"    
  show Green = "Green light"

class YesNo a where
  yesno :: a -> Bool

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo [a] where
  yesno [] = False
  yesno _ = True

instance YesNo Bool where
  yesno = id

instance YesNo (Maybe a) where
  yesno (Just _) = True
  yesno Nothing = False

instance YesNo (Tree a) where
  yesno EmptyTree = True
  yesno _ = False  

instance YesNo TrafficLight where
  yesno Red = False
  yesno _ = True  

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = 
  if yesno yesnoVal then yesResult
                    else noResult
