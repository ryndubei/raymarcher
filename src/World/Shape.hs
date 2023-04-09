{-# LANGUAGE NamedFieldPuns #-}
module World.Shape (Shape, IsShape(..), toShape) where

import Graphics.Gloss.Data.Color ( Color )

type Point = (Double, Double, Double)

class IsShape a where
  {-# MINIMAL colour, (distance | distanceSq) #-}
  -- | Get the colour of the nearest point on a shape.
  colour :: a -> Point -> Color
  -- | Get the signed distance function from a shape.
  distance :: a -> Point -> Double
  -- | Get the signed distance function from a shape, with a squared
  -- absolute value.
  distanceSq :: a -> Point -> Double

  distance s p
    | d < 0 = -sqrt (-d)
    | otherwise = sqrt d
    where d = distanceSq s p
  distanceSq s p 
    | d < 0 = -(d*d)
    | otherwise = d*d
    where d = distance s p

-- | Conversion to the generic shape type, which allows us to do
-- transformations on the distance function.
toShape :: IsShape a => a -> Shape
toShape s = Shape (distance s) (colour s)

-- | A Shape is something that possesses a distance function and
-- a colour.
data Shape = Shape 
  { shapeDistance :: Point -> Double
  , shapeColour :: Point -> Color
  }

instance IsShape Shape where
  distance Shape{shapeDistance} = shapeDistance
  colour Shape{shapeColour} = shapeColour