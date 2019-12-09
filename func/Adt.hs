import qualified Data.Map as Map

data Person = Person {firstName :: String
                     ,lastName :: String
                     ,age :: Int
                     } deriving (Eq, Show,Read)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

data QEither a b = QLeft a | QRight b 
                  deriving (Eq, Ord, Read, Show)

data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)
lockers :: LockerMap
lockers = Map.fromList [(100, (Taken, "Zelda"))
                       ,(101, (Taken, "Link"))
                       ,(102, (Free, "ZD123"))
                       ,(103, (Taken, "Mario"))
                       ,(104, (Free, "DK197"))]

infixr 5 :-:
data List a = Empty | a :-: (List a) 
              deriving (Show, Read, Eq, Ord)
infixr 5 ^++
(^++) :: List a -> List a -> List a
Empty ^++ ys = ys
(x :-: xs) ^++ ys = x :-: (xs ^++ ys)

mikeD = Person "Michael" "Diamond" 43
adRock = Person "Adam" "Horovitz" 41
mca = Person "Adam" "Yauch" 44

mysteryDude = "Person {firstName = \"Michael\"" ++ 
                      ", lastName = \"Diamond\"" ++
                      ", age =43}"

lockerLookup :: Int -> LockerMap -> QEither String Code
lockerLookup lockerNumber map = case Map.lookup lockerNumber map of
                                Nothing -> QLeft $ "Locker " ++ show lockerNumber ++ " doesn't exist!"
                                Just (state, code) -> if state /= Taken 
                                                        then QRight code
                                                        else QLeft $ "Locker " ++ show lockerNumber ++ " is already taken!"


