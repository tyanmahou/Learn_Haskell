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