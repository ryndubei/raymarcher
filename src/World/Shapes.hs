{-# LANGUAGE NamedFieldPuns #-}
module World.Shapes (Sphere(..), Plane(..), Cube(..)) where

import World.Shape
import Graphics.Gloss.Accelerate.Raster.Field (Colour)
import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.Linear as AL
import qualified Linear as L
import Data.Array.Accelerate.Linear ()

data Sphere = Sphere 
  { sphereRadius :: A.Exp Double
  , spherePosition :: A.Exp Point
  , sphereColour :: A.Exp Colour
  }

instance IsShape Sphere where
  colour Sphere{sphereColour} = const sphereColour
  distance Sphere{sphereRadius, spherePosition} v = 
    AL.distance v spherePosition - sphereRadius

data Plane = Plane 
  { planeNormal :: A.Exp Point
  , planeOffset :: A.Exp Double
  , planeColour :: A.Exp Point -> A.Exp Colour
  }

instance IsShape Plane where
  colour Plane{planeColour} = planeColour
  distance Plane{planeNormal, planeOffset} v = 
    ((v `AL.dot` planeNormal) - planeOffset) / sqrt (planeNormal `AL.dot` planeNormal)

data Cube = Cube
  { cubeRadius :: A.Exp Double
  , cubePosition :: A.Exp Point
  , cubeColour :: A.Exp Colour
  }

instance IsShape Cube where
  colour Cube{cubeColour} = const cubeColour
  distance Cube{cubeRadius, cubePosition} p =
    let (L.V3 x y z) = A.unlift p
        (L.V3 a b c) = A.unlift cubePosition
     in foldr1 A.max [abs (x - a), abs (y - b), abs (z - c)] - cubeRadius
