module Main where
import World
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

pVel :: Float
pVel   = 310.0                                 -- paddle velocity

window :: Display
window = InWindow "Gloss Pong" (800, 600) (10, 10)

main :: IO ()
main = play 
       window
       black
       60
       genesis
       drawEntities
       handleKey
       moveEntities
       
-- handle a or d keys to move left or right
handleKey :: Event -> World -> World
handleKey (EventKey (Char 'a') Down _ _) w            = movePaddle w (-pVel)
handleKey (EventKey (Char 'a') Up _ _) w              = movePaddle w 0.0
handleKey (EventKey (SpecialKey KeyLeft) Down _ _) w  = movePaddle w (-pVel)
handleKey (EventKey (SpecialKey KeyLeft) Up _ _) w    = movePaddle w 0.0

handleKey (EventKey (Char 'd') Down _ _) w            = movePaddle w pVel
handleKey (EventKey (Char 'd') Up _ _) w              = movePaddle w 0.0
handleKey (EventKey (SpecialKey KeyRight) Down _ _) w = movePaddle w pVel
handleKey (EventKey (SpecialKey KeyRight) Up _ _) w   = movePaddle w 0.0

handleKey _ w = w