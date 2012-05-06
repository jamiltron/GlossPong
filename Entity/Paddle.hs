module Entity.Paddle where

import Graphics.Gloss
import Entity

--            Paddle x     y     w     h     xv
data Paddle = Paddle Float Float Float Float Float
                                

-- change the  direction of the paddle, return a paddle that needs a new x-vel
changeDir :: Paddle -> Float -> Paddle
changeDir (Paddle x y w h v) = Paddle x y w h


paddleColor = Color (makeColor 0.6 0.6 0.6 1.0)

instance Entity Paddle where
  center (Paddle x y _ _ _)  = (x, y)
  left   (Paddle x _ w _ _)  = x - w / 2.0
  right  (Paddle x _ w _ _)  = x + w / 2.0
  top    (Paddle _ y _ h _)  = y + h / 2.0
  bottom (Paddle _ y _ h _)  = y - h / 2.0
  width  (Paddle _ _ w _ _)  = w
  height (Paddle _ _ _ h _)  = h
  xvel   (Paddle _ _ _ _ xv) = xv
  yvel   _                   = 0
  
  move (Paddle x y w h v) time
    | x <= (-336) && v <= 0 = Paddle x y w h v
    | x >=   336  && v >= 0 = Paddle x y w h v
    | otherwise = Paddle (x + v * time) y w h v 

  render (Paddle x y w h _) = paddleColor
                $ Translate x y
                $ rectangleSolid w h
