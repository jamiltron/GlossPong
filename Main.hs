module Main where

import World
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

pVel   = 4.0
window = InWindow "Gloss Pong" (800, 600) (10, 10)

main = play 
       window
       black
       60
       genesis
       drawEntities
       handleKey
       moveEntities
       
handleKey :: Event -> World -> World
handleKey (EventKey (Char 'a') Down nomods _) w = movePaddle w (-pVel)
handleKey (EventKey (Char 'a') Up nomods   _) w = movePaddle w 0.0
handleKey (EventKey (Char 'd') Down nomods _) w = movePaddle w pVel
handleKey (EventKey (Char 'd') Up nomods   _) w = movePaddle w 0.0
handleKey _ w = w
  where nomods = Modifiers Up Up Up