module Events (keyHandler,newgame) where
import Graphics.UI.SDL as SDL hiding (init,update)
import Cards
import Logic
import Gui as GUI
import Control.Monad (unless)
import System.Random (randomRIO)

checkEvents :: String -> String -> ([Card]->[Card]->Playground->Bool) -> Playground -> Surface -> Playground
checkEvents from to rule pg screen
  | ruleTrue && from `elem` ["reserve","talon"] = update [(from,tail ffrom),(to,head ffrom:fto)] pg
  | ruleTrue && Prelude.init to=="found" = update [(from,tail ffrom),(to,head ffrom:fto)] pg
  | ruleTrue = update [(from,[]),(to,ffrom++fto)] pg
  | otherwise = pg
  where
    ffrom = fetch from pg
    fto   = fetch to pg
    ruleTrue = rule ffrom fto pg

rollHand :: Playground -> Playground
rollHand pg | lha == 0  = update [("talon",[]),("hand",reverse tal)] pg
            | lha <= 3  = update [("hand",[]), ("talon",reverse ha++tal)] pg
            | otherwise = update [("talon",reverse (take 3 ha)++tal),("hand",drop 3 ha)] pg
            where
              lha = length ha
              ha  = hand pg
              tal = talon pg

newgame :: IO ()
newgame = getVideoSurface >>= \screen ->
  drawBackground screen >>
  shuffle stapel >>= \stack ->
  let pg = playground stack
  in drawField pg screen >> keyHandler pg

shuffle :: [a] -> IO [a]
shuffle = shuffle' []
  where
    shuffle' acc []  = return acc
    shuffle' acc lst = randomRIO (0,length lst-1)>>= \count->
      let (skip, r:rs) = splitAt count lst
      in shuffle' (r:acc) (skip++rs)

keyHandler :: Playground -> IO ()
keyHandler pg = getVideoSurface >>= \screen ->
  drawField pg screen >>
  waitEvent >>= \e->
  unless (e == Quit) $
  let ekey = extractKey e in
    basicHandler ekey pg screen
      ( if ekey `elem` [SDLK_a,SDLK_s,SDLK_d,SDLK_f,SDLK_g,SDLK_t]
        then waitEvent >>= \f ->
          let fkey = extractKey f in
            basicHandler fkey pg screen
              ( if fkey `elem` [SDLK_a,SDLK_s,SDLK_d,SDLK_f,SDLK_q,SDLK_w,SDLK_e,SDLK_r]
                then
                  let iekey = keyToIdentifier ekey
                      ifkey = keyToIdentifier fkey
                      ce rule = checkEvents iekey ifkey rule pg screen
                   in case (iekey, init ifkey) of
                      (_,"found")   -> keyHandler.ce $ foundationRule
                      ("talon",_)   -> keyHandler.ce $ talonTableauRule
                      ("reserve",_) -> keyHandler.ce $ reserveTableauRule
                      (_,_)         -> keyHandler.ce $ tableauRule
                else keyHandler pg)
        else keyHandler pg)
  where
    extractKey :: Event -> SDLKey
    extractKey (SDL.KeyUp (Keysym s _ _)) = s
    extractKey _ = SDLK_UNKNOWN
    validKey = Prelude.flip elem [SDLK_a,SDLK_s,SDLK_d,SDLK_f,SDLK_g,SDLK_t]
    basicHandler i pg screen cont = case i of
      SDLK_ESCAPE -> return ()
      SDLK_SPACE -> keyHandler $ rollHand pg
      SDLK_n -> newgame
      SDLK_F12 -> toggleFullscreen screen >> keyHandler pg
      _ -> cont

keyToIdentifier :: SDLKey -> String
keyToIdentifier SDLK_a = "tableau1"
keyToIdentifier SDLK_s = "tableau2"
keyToIdentifier SDLK_d = "tableau3"
keyToIdentifier SDLK_f = "tableau4"
keyToIdentifier SDLK_q = "found1"
keyToIdentifier SDLK_w = "found2"
keyToIdentifier SDLK_e = "found3"
keyToIdentifier SDLK_r = "found4"
keyToIdentifier SDLK_t = "reserve"
keyToIdentifier SDLK_g = "talon"
keyToIdentifier _      = " "
