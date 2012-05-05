module Entity.Ball where

import Graphics.Gloss
import Data.Tuple (uncurry)
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

-- move the ball, checking for collision against another entity
ballMove :: (Entity a) => Ball -> a -> Ball
ballMove ball other = let (Ball x y r xv yv) = (bounce ball other)
                      in Ball (x + xv) (y + yv) r xv yv
                         
-- do all of our neccessary bounce checks and reflection
bounce :: Entity a => Ball -> a -> Ball
bounce ball other = bounceY  . bounceX  $ bounceOther other ball

-- check if we need to bounce off of a left or right wall
bounceX :: Ball -> Ball
bounceX ball@(Ball x _ _ _ _)
  | x <= (-390.0) = reflectX ball (-390.0)
  | x >= 390.0    = reflectX ball 390.0
  | otherwise     = ball
                    
-- check if we need to bounce off of the top or bottom
bounceY :: Ball -> Ball
bounceY ball@(Ball _ y _ _ _)
  | y <= (-290.0) = reflectY ball (-290.0)
  | y >= 290.0    = reflectY ball 290.0
  | otherwise     = ball

-- check if we are colliding with another entity                
bounceOther :: Entity a => a -> Ball -> Ball
bounceOther other ball = if Entity.aabb other ball
                         then reflectOther other ball
                         else ball

-- bounce the ball horizontally
reflectX :: Ball -> Float -> Ball
reflectX (Ball x y r xv yv) x' = Ball x' y r (-1.0 * xv) yv 

-- bounce the ball vertically
reflectY :: Ball -> Float -> Ball
reflectY (Ball x y r xv yv) y' = Ball x y' r xv (-1.0 * yv)

-- if colliding with another, check where it needs to be reflected
-- this calculates which side is in collision, and then figures
-- out which edge(s) the ball needs to be reflected off of
reflectOther :: Entity a => a -> Ball -> Ball
reflectOther other ball 
  -- if the colliding edges are equal, reflect both
  | min yt yb == min xl xr = reflectBoth
                             
  -- if the top is the colliding edge, figure out if we need to reflect l or r
  | yt < yb   = if xr < xl then topRight    else topLeft
                                                 
  -- otherwise, we are colliding on the bottom
  | otherwise = if xr < xl then bottomRight else bottomLeft
  where
    yt = top other - bottom ball
    yb = top ball - bottom other 
    xl = right ball - left other
    xr = right other - left ball
    hb = height ball / 2.0
    wb = height ball / 2.0
    topLeft     = if yt < xl 
                  then reflectY ball (top other  + hb)
                  else reflectX ball (left other - wb)
                       
    topRight    = if yt < xr
                  then reflectY ball (top other   + hb)
                  else reflectX ball (right other + wb)
                       
    bottomLeft  = if yb < xl 
                  then reflectY ball (bottom other - hb)
                  else reflectX ball (left other - wb)
                       
    bottomRight = if yb < xr
                  then reflectY ball (bottom other - hb)
                  else reflectX ball (right other + wb)
                       
    reflectBoth = let yPos = if yt < yb then top other  + hb else bottom other - hb
                      xPos = if xr < xl then left other - wb else right other  + wb
                  in reflectY (reflectX ball xPos) yPos




                            
                
  