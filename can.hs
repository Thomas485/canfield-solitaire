module Main (main) where

import System.Environment (getArgs)
import Gui (start,end)
import Events (newgame)

main :: IO ()
main  = getArgs >>= start >> newgame >> end
