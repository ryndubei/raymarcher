module World.Shapes where

import World.Shape

data Sphere = Sphere 
  { radius :: Double
  , position :: (Double, Double, Double) 
  }
