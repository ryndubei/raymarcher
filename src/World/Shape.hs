{-# LANGUAGE NamedFieldPuns #-}
module World.Shape (Shape, IsShape(..), toShape, Point, normal) where

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

  distance s x
    | d < 0 = -sqrt (-d)
    | otherwise = sqrt d
    where d = distanceSq s x
  distanceSq s x 
    | d < 0 = -(d*d)
    | otherwise = d*d
    where d = distance s x

-- | Approximate the unit normal vector to the nearest surface point
-- on a shape.
normal :: IsShape a => a -> Point -> Point
-- TODO: complete definition
normal s x = undefined

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

instance Semigroup Shape where
  s1 <> s2 = Shape d c 
    where
      d x = min (shapeDistance s1 x) (shapeDistance s2 x)
      c x
        | shapeDistance s1 x < shapeDistance s2 x = shapeColour s1 x
        | otherwise = shapeColour s2 x
