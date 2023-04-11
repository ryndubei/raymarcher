{-# LANGUAGE NamedFieldPuns #-}
module World.Shapes (Sphere(..), Plane(..), Cube(..)) where

import World.Shape.Temporary
import Graphics.Gloss.Data.Color (Color)
import qualified Linear as L

data Sphere = Sphere 
  { sphereRadius :: Double
  , spherePosition :: Point
  , sphereColour :: Color
  }

instance IsShape Sphere where
  colour Sphere{sphereColour} = const sphereColour
  distance Sphere{sphereRadius, spherePosition} v = 
    L.distance v spherePosition - sphereRadius

data Plane = Plane 
  { planeNormal :: Point
  , planeOffset :: Double
  , planeColour :: Point -> Color
  }

instance IsShape Plane where
  colour Plane{planeColour} = planeColour
  distance Plane{planeNormal, planeOffset} v = 
    ((v `L.dot` planeNormal) - planeOffset) / sqrt (planeNormal `L.dot` planeNormal)

data Cube = Cube
  { cubeRadius :: Double
  , cubePosition :: Point
  , cubeColour :: Color
  }

instance IsShape Cube where
  colour Cube{cubeColour} = const cubeColour
  distance Cube{cubeRadius, cubePosition} (L.V3 x y z) =
    let (L.V3 a b c) = cubePosition
     in maximum [abs (x - a), abs (y - b), abs (z - c)] - cubeRadius
