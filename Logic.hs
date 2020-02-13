module Logic where
import Cards

differentColors :: Card -> Card -> Bool
x `differentColors` y = pred (symbol x) == symbol y || succ (symbol x) == symbol y

isSuccOf :: Card -> Card -> Bool
x `isSuccOf` y = pred (rang x) == rang y

isPredOf :: Card -> Card -> Bool
x `isPredOf` y = succ (rang x) == rang y

tableauRule :: [Card] -> [Card] -> Playground -> Bool
tableauRule [] _ _ = False
tableauRule _ [] _ = True
tableauRule xs (y:_) _ = let lastx = last xs 
  in lastx `isPredOf` y && lastx `differentColors` y

reserveTableauRule :: [Card] -> [Card] -> Playground -> Bool
reserveTableauRule [] _ _ = False
reserveTableauRule _ [] _ = True
reserveTableauRule (x:_) (y:_) _ =
  x `isPredOf` y && x `differentColors` y

talonTableauRule :: [Card] -> [Card] -> Playground -> Bool
talonTableauRule [] _ _ = False
talonTableauRule _ [] pg = null (reserve pg) 
talonTableauRule (x:_) (y:_) _ =
  x `isPredOf` y && x `differentColors` y

foundationRule :: [Card] -> [Card] -> Playground-> Bool
foundationRule [] _ _ = False
foundationRule (x:_) [] pg = level pg == rang x
foundationRule (x:_) (y:_) _ = x `isSuccOf` y && symbol x == symbol y
