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
