module Entity.Ball where

import Graphics.Gloss
import Data.Tuple (uncurry)
import Debug.Trace
import Entity
      
--          Ball x     y     r     xv    yv
data Ball = Ball Float Float Float Float Float

instance Entity Ball where
  center (Ball x y _ _ _) = (x, y)
  left   (Ball x _ r _ _) = x - r
  right  (Ball x _ r _ _) = x + r
  top    (Ball _ y r _ _) = y + r
  bottom (Ball _ y r _ _) = y - r
  width  (Ball _ _ r _ _) = 2.0 * r
  height (Ball _ _ r _ _) = 2.0 * r
  xvel   (Ball _ _ _ v _) = v
  yvel   (Ball _ _ _ _ v) = v
  
  move ball = let (Ball x y r xv yv) = ball
              in Ball (x + xv) (y + yv) r xv yv
  
  render ball = Color (makeColor 0.7 0.2 0.3 1.0)
              $ uncurry Translate  (center ball)
              $ ThickCircle 1.0 (width ball)

ballMove ball other = let (Ball x y r xv yv) = (bounce ball other)
                      in Ball (x + xv) (y + yv) r xv yv

reflectX :: Ball -> Float -> Ball
reflectX (Ball x y r xv yv) x' = Ball x' y r (-1.0 * xv) yv 
                   
reflectY :: Ball -> Float -> Ball
reflectY (Ball x y r xv yv) y' = Ball x y' r xv (-1.0 * yv)

reflectOther :: Entity a => a -> Ball -> Ball
reflectOther other ball 
  | yb > yt   = if xr < xl then topRight    other ball else topLeft    other ball
  | otherwise = if xr < xl then bottomRight other ball else bottomLeft other ball
  where
    yt = (top other - bottom ball) 
    yb = (top ball - bottom other) 
    xl = (right ball - left other)
    xr = (right other - left ball)
    topLeft other ball     = if yt < xl 
                             then reflectY ball (top other + (height ball) / 2.0)
                             else reflectX ball (left other + (width ball) / 2.0)
    topRight other ball    = if yt < xr
                             then reflectY ball (top other + (height ball) / 2.0)
                             else reflectX ball (right other + (width ball) / 2.0)
    bottomLeft other ball  = if yb < xl 
                             then reflectY ball (bottom other - (height ball) / 2.0)
                             else reflectX ball (left other + (width ball) / 2.0)
    bottomRight other ball = if yb < xr
                             then reflectY ball (bottom other - (height ball) / 2.0)
                             else reflectX ball (right other + (width ball) / 2.0)




bounceX :: Ball -> Ball
bounceX ball@(Ball x _ _ _ _)
  | x <= (-390.0) = reflectX ball (-390.0)
  | x >= 390.0    = reflectX ball 390.0
  | otherwise     = ball
                                                  
bounceY :: Ball -> Ball
bounceY ball@(Ball _ y _ _ _)
  | y <= (-290.0) = reflectY ball (-290.0)
  | y >= 290.0 = reflectY ball 290.0
  | otherwise = ball
                
bounceOther :: Entity a => a -> Ball -> Ball
bounceOther other ball = if (Entity.aabb other ball) 
                         then reflectOther other ball
                         else ball

bounce :: Entity a => Ball -> a -> Ball
bounce ball other = (bounceY  (bounceX  (bounceOther other ball)))
                            
                
  