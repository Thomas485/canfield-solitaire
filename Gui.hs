module Gui where
import Data.Char (digitToInt)
import Data.Map
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image (load)
import Cards

gfx,suf,theme :: String
gfx   = "gfx/"
suf   = ".jpg"
theme = "tmp2/"

feld :: IO Surface
feld = load $ gfx++theme++"field"++suf

karte :: String -> IO Surface
karte x = load $ gfx++theme++"cards/"++x++suf

cardImage :: Card -> IO Surface
cardImage Background       = karte "background"
cardImage (Heart   `Of` x) = karte $ 'h':r x
cardImage (Diamond `Of` x) = karte $ 'd':r x
cardImage (Club    `Of` x) = karte $ 'c':r x
cardImage (Spade   `Of` x) = karte $ 's':r x

r :: Rang->String
r a = snd.head.dropWhile ((a/=).fst) $ cycle $ zip rangList stringList
  where
    rangList   = Cards.successorRangList
    stringList = ["a","2","3","4","5","6","7","8","9","t","j","q","k"]

checkArgs :: [String] -> IO ()
checkArgs argv
  | "-f"`elem`argv || "--fullscreen"`elem`argv = getVideoSurface >>= toggleFullscreen
  | "-g"`elem`argv || "--grab"`elem`argv = grabInput True
  | otherwise = return ()

start :: [String] -> IO ()
start argv = SDL.init [InitVideo] >>
  setVideoMode 800 600 32 [] >>
  setCaption "Canfield" "" >>
  enableEvent SDLKeyDown False >>
  enableEvent SDLNoEvent False >>
  enableEvent SDLMouseMotion False >>
  enableEvent SDLMouseButtonDown False >>
  enableEvent SDLMouseButtonUp False >>
  checkArgs argv
end :: IO ()
end = SDL.quit

drawCard :: Card -> Int -> Int -> Surface -> IO Bool
drawCard card x y screen = cardImage card >>= \c ->
  blitSurface c (Just (Rect 0 0 80 122)) screen (Just (Rect x y 80 122)) >> freeSurface c >> return True

fillCardBackground screen x y w h = fillRect screen (Just (Rect x y w h)) (Pixel 0x00ffffff)

drawReserve :: [Card] -> Surface -> IO Bool
drawReserve (x:_) screen = drawCard x 20 220 screen
drawReserve [] screen = fillCardBackground screen 20 220 80 122

drawFoundation :: Int -> [Card] -> Surface -> IO Bool
--150+130*(n-1)= 20+130*n
drawFoundation n (x:_) screen = drawCard x (20+130*n) 20 screen
drawFoundation _ [] _ = return False

drawHand :: [Card] -> Surface -> IO Bool
drawHand [] screen = fillCardBackground screen 670 390 80 122
drawHand _ screen = drawCard Background 670 390 screen

drawTalon :: [Card] -> Surface -> IO Bool
drawTalon [] screen = fillCardBackground screen 670 220 80 122
drawTalon (x:_) screen = drawCard x 670 220 screen

drawTableau :: Int -> [Card] -> Surface -> IO Bool
--150+130*(n-1)= 20+130*n ; 220+20*(length l-1)=200+20*(length l)
drawTableau n l@(x:xs) screen = drawTableau n xs screen >>
  drawCard x (20+130*n) (200+20*length l) screen
drawTableau _ [] _ = return False -- Tableau n ist leer

drawBackground :: Surface -> IO Bool
drawBackground screen = feld >>= \back -> blitSurface back Nothing screen Nothing >> freeSurface back >> return True

drawField :: Playground -> Surface -> IO ()
drawField pg screen =
  mapM_ (\i->fillCardBackground screen i 220 80 350)
    [150,280,410,540]>>
  drawReserve (reserve pg) screen >>
  mapM_ (\(n,f)->drawFoundation n (f pg) screen)
    [(1,found1),(2,found2),(3,found3),(4,found4)]>>
  mapM_ (\(n,t)->drawTableau n (t pg) screen)
    [(1,tableau1),(2,tableau2),(3,tableau3),(4,tableau4)]>>
  drawHand (hand pg) screen >>
  drawTalon (talon pg) screen >>
  SDL.flip screen
