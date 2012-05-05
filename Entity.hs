module Entity where
import Graphics.Gloss

type XY = (Float, Float)

class Entity a where
  center      :: a -> XY
  left        :: a -> Float
  right       :: a -> Float
  top         :: a -> Float
  bottom      :: a -> Float
  width       :: a -> Float
  height      :: a -> Float
  xvel        :: a -> Float
  yvel        :: a -> Float
  move        :: a -> a
  render      :: a -> Picture
  
-- basic bounding box collision
aabb e1 e2 = not (xCheck e1 e2) && not (yCheck e1 e2)
  where
    xCheck e1 e2 = right e1  < left e2 || left e1  > right e2
    yCheck e1 e2 = bottom e1 > top e2  || top e1   < bottom e2
    
