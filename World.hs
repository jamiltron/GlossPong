module World where

import Graphics.Gloss
import Debug.Trace
import Entity
import Entity.Paddle
import Entity.Ball

data World = World { paddle :: Paddle 
                   , ball   :: Ball
                   , score  :: Int
                   }

genesis :: World
genesis =  World { paddle = Paddle 0.0 (-240.0) 128 32 0.0
                 , ball   = Ball 0.0 0.0 8.0 3.0 3.0
                 , score  = 0 }
           
drawText message = Translate (-390) 288
                 $ Color white
                 $ Scale 0.1 0.1
                 $ Text $ message

drawEntities :: World -> Picture
drawEntities world = Pictures [ render (paddle world)
                              , render (ball world)
                              , drawText 
                                $ "Score: " ++ (show (score world))]

moveEntities :: Float -> World -> World
moveEntities time world = let p = move (paddle world)
                              b   = ballMove (ball world) (paddle world)
                              s  = checkScore (ball world) b (score world)
                          in World p b s

movePaddle :: World -> Float -> World
movePaddle world vel = let p = (paddle world)
                           b = (ball world)
                           s = (score world)
                       in World (changeDir p vel) b s
                          
checkScore oldball newball score
  | bottom newball <= (-290.0) && 
    bottom oldball > (-290.0) = 0
                                
  | top newball >= 290.0 &&  
    top oldball < 290.0       = score + 1
  | otherwise                 = score