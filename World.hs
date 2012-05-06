module World where

import Graphics.Gloss
import Entity
import Entity.Paddle
import Entity.Ball

type Score = Int

data World = World Paddle Ball Score

-- starting world
genesis :: World
genesis =  World (Paddle 0.0 (-240.0) 128 32 0.0)
                 (Ball 0.0 0.0 8.0 308.0 308.0)
                 0
           
-- display text in the top-left corner
drawText message = Translate (-390) 288
                 $ Color white
                 $ Scale 0.1 0.1
                 $ Text message

-- draw the paddle, ball, and score
drawEntities :: World -> Picture
drawEntities (World p b s) = Pictures [ render p
                                      , render b
                                      , drawText 
                                        $ "Score: " ++ show s]

-- move the paddle based on its velocity, check if the ball bounces, and check
-- if score needs to be reset or incremented
moveEntities :: Float -> World -> World
moveEntities time (World p b s) = let p' = move p time
                                      b' = ballMove b p time
                                      s'  = checkScore b b' s
                                  in World p' b' s'

-- Change the paddle's velocity to the given float 
movePaddle :: World -> Float -> World
movePaddle (World p b s) vel =  World (changeDir p vel) b s
                          
-- See if the old ball was hitting the correct wall, then once the updated
-- version of the ball has bounced from the wall, update score accourdingly
checkScore :: Ball -> Ball -> Score -> Score
checkScore oldball newball score
  -- if the ball has hit the bottom, reset score
  | bottom newball <= (-290.0) && 
    bottom oldball > (-290.0) = 0
                                
  -- if the ball has hit the ceiling, increment score
  | top newball >= 290.0 &&  
    top oldball < 290.0       = score + 1
                                
  | otherwise                 = score