{-# LANGUAGE NamedFieldPuns #-}
module World.Shapes (Sphere(..), Plane(..)) where

import World.Shape
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
