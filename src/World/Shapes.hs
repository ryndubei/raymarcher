{-# LANGUAGE NamedFieldPuns #-}
module World.Shapes (Sphere(..), Plane(..)) where

import World.Shape
import Graphics.Gloss.Data.Color (Color)

data Sphere = Sphere 
  { sphereRadius :: Double
  , spherePosition :: Point
  , sphereColour :: Color
  }

instance IsShape Sphere where
  colour Sphere{sphereColour} = const sphereColour
  distance Sphere{sphereRadius, spherePosition} (x,y,z) = 
    sqrt (dx*dx + dy*dy + dz*dz) - sphereRadius
    where 
      (x', y', z') = spherePosition
      (dx, dy, dz) = (x - x', y - y', z - z')

data Plane = Plane 
  { planeNormal :: Point
  , planeOffset :: Double
  , planeColour :: Color
  }

instance IsShape Plane where
  colour Plane{planeColour} = const planeColour
  distance Plane{planeNormal, planeOffset} (x,y,z) = 
    (x * a + y * b + z * c - planeOffset) / sqrt (a*a + b*b + c*c)
    where 
      (a, b, c) = planeNormal
