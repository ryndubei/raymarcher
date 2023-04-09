{-# LANGUAGE NamedFieldPuns #-}
module World.Shapes (Sphere) where

import World.Shape
import Graphics.Gloss.Data.Color (Color)

data Sphere = Sphere 
  { sphereRadius :: Double
  , spherePosition :: Point
  , sphereColour :: Color
  }

instance IsShape Sphere where
  colour Sphere{sphereColour} = const sphereColour
  distanceSq Sphere{sphereRadius, spherePosition} (x,y,z) = 
    (dx*dx + dy*dy + dz*dz) - (sphereRadius * sphereRadius)
    where 
      (x', y', z') = spherePosition
      (dx, dy, dz) = (x - x', y - y', z - z')
