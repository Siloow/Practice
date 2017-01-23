data Trivial =
    Trivial'

instance Eq Trivial where
    Trivial' == Trivial' = True

data DayOfWeek =
    Mon | Tue | Weds | Thu | Fri | Sat | Sun

data Date =
    Date DayOfWeek Int

instance Eq DayOfWeek where
    (==) Mon Mon    = True
    (==) Tue Tue    = True
    (==) Weds Weds  = True
    (==) Thu Thu    = True
    (==) Fri Fri    = True
    (==) Sat Sat    = True
    (==) Sun Sun    = True
    (==) _ _        = False

-- instance Eq Date where
--     (==) (Date weekday dayOfMonth)
--          (Date weekday' dayOfMonth') =
--       weekday == weekday' && dayOfMonth == dayOfMonth'

-- instance Eq Date where
--     (Date weekday dayOfMonth) == (Date weekday' dayOfMonth') = weekday == weekday' && dayOfMonth == dayOfMonth'

instance Eq Date where
    (==) (Date weekday dayOfMonth) (Date weekday' dayOfMonth') =
        (==) weekday weekday' &&
        (==) dayOfMonth dayOfMonth'


nonsense :: Bool -> Integer
nonsense True = 805
nonsense False = 31337

curriedFunction :: Integer -> Bool -> Integer
curriedFunction i b =
    i + (nonsense b)

uncurriedFunction :: (Integer, Bool) -> Integer
uncurriedFunction (i, b) =
    i + (nonsense b)

anonymous :: Integer -> Bool -> Integer
anonymous = \i b -> i + (nonsense b)

anonNested :: Integer -> Bool -> Integer
anonNested =
    \i -> \b -> i + (nonsense b)

f :: Num a => a -> a -> a
f x y = x + y + 3

newtype Username = Username String
newtype AccountNumber = AccountNumber Integer

data User = UnregisteredUser
          | RegisteredUser Username AccountNumber


printUser :: User -> IO ()
printUser UnregisteredUser = putStrLn "UnregisteredUser"
printUser (RegisteredUser (Username name)
                          (AccountNumber acctNum))
          = putStrLn $ name ++ " " ++ show acctNum

data WherePenguinsLive =
     Galapagos
   | Antartica
   | Australia
   | SouthAfrica
   | SouthAmerica
   deriving (Eq, Show)

data Penguin = 
  Peng WherePenguinsLive
  deriving (Eq, Show)

isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica _ = False

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng whereitlives) = whereitlives

funcZ x =
    case x + 1 == 1 of
        True -> "KEk"
        False -> "Top"

pal xs =
    case xs == reverse xs of
        True -> "Yes"
        False -> "No"