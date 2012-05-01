module World where

import Graphics.Gloss
import Entity
import Entity.Paddle
import Entity.Ball

data World = World { paddle :: Paddle 
                   , ball   :: Ball }

genesis :: World
genesis =  World { paddle = Paddle 0.0 (-240.0) 128 32 0.0
                 , ball   = Ball 0.0 0.0 8.0 3.0 3.0 }

drawEntities :: World -> Picture
drawEntities world = Pictures [ render (paddle world)
                              , render (ball world)]

moveEntities :: Float -> World -> World
moveEntities time world = World 
                          (move (paddle world)) 
                          (ballMove (ball world) (paddle world))

movePaddle :: World -> Float -> World
movePaddle world vel = let p = (paddle world)
                           b = (ball world)
                       in (World (changeDir p vel) b)