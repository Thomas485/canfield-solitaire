module Cards where
import Control.Monad (liftM2)
import Data.Function (on)

data Rang   = Ace|R2|R3|R4|R5|R6|R7|R8|R9|R10|Jack|Queen|King deriving (Eq, Show, Ord)
data Symbol = Diamond|Spade|Heart|Club deriving (Eq, Show, Ord)
data Card   = Symbol `Of` Rang|Background deriving (Show)

rang :: Card -> Rang
rang (_ `Of` y) = y
rang _          = undefined

symbol :: Card -> Symbol
symbol (x `Of` _) = x
symbol _          = undefined

instance Eq Card where
  (==) = (==) `on` rang
  (/=) = (/=) `on` rang 

instance Ord Card where
  (<)  = (<)  `on` rang
  (<=) = (<=) `on` rang
  (>)  = (>)  `on` rang
  (>=) = (>=) `on` rang

instance Enum Symbol where
  fromEnum a = snd.head.dropWhile ((a/=).fst) $ cycle $ zip successorSymbolList [1..4]
  toEnum   a = fst.head.dropWhile ((a/=).snd) $ cycle $ zip successorSymbolList [1..4]
  succ x = head.tail.dropWhile (/=x) $ cycle successorSymbolList
  pred x = head.tail.dropWhile (/=x) $ cycle $ reverse successorSymbolList

successorSymbolList :: [Symbol]
successorSymbolList = [Diamond,Spade,Heart,Club]
 
instance Enum Rang where
  fromEnum a = snd.head.dropWhile ((a/=).fst) $ cycle $ zip successorRangList [1..13]
  toEnum   a = fst.head.dropWhile ((a/=).snd) $ cycle $ zip successorRangList [1..13]
  succ x = head.tail.dropWhile (/=x) $ cycle successorRangList
  pred x = head.tail.dropWhile (/=x) $ cycle $ reverse successorRangList

successorRangList :: [Rang]
successorRangList = [Ace,R2,R3,R4,R5,R6,R7,R8,R9,R10,Jack,Queen,King]

stapel :: [Card]
stapel = liftM2 Of [Diamond ..Club] [Ace .. King]

type Playground = [(String,[Card])]

update :: [(String,[Card])] -> Playground -> Playground
update [] liste = liste
update (element@(key,_):xs) liste =update xs $ element:filter (\(k,_)->k/=key) liste

fetch :: String -> Playground -> [Card]
fetch name ((a,b):xs)
  | name == a = b
  | otherwise = fetch name xs
fetch _ _ = []

reserve,talon,found1,found2,found3,found4,tableau1,tableau2,tableau3,tableau4,hand :: Playground -> [Card]
reserve  = fetch "reserve"
talon    = fetch "talon"
found1   = fetch "found1"
found2   = fetch "found2"
found3   = fetch "found3"
found4   = fetch "found4"
tableau1 = fetch "tableau1"
tableau2 = fetch "tableau2"
tableau3 = fetch "tableau3"
tableau4 = fetch "tableau4"
hand     = fetch "hand"

level :: Playground -> Rang
level = rang . head . fetch "level"

playground :: [Card] -> Playground
playground stack =
  [ ("reserve",take 13 stack)
  , ("talon",[])
  , ("level",take 1 $ drop 13 stack)
  , ("found1",take 1 $ drop 13 stack)
  , ("found2",[])
  , ("found3",[])
  , ("found4",[])
  , ("tableau1",take 1 $ drop 14 stack)
  , ("tableau2",take 1 $ drop 15 stack)
  , ("tableau3",take 1 $ drop 16 stack)
  , ("tableau4",take 1 $ drop 17 stack)
  , ("hand", drop 18 stack)
  ]
