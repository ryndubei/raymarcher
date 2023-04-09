{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE NamedFieldPuns #-}
module World.Shape (Shape, IsShape(..), toShape) where

import Graphics.Gloss.Data.Color

newtype Point d = Point (d,d,d)

instance Functor Point where
  fmap f (Point (x,y,z)) = Point (f x, f y, f z)

class IsShape a where
  {-# MINIMAL colour, (distance | distanceSq | distanceSqDouble) #-}
  -- | Get the colour of the nearest point on a shape.
  colour :: RealFloat d => a -> Point d -> Color
  -- | Get the signed distance function from a shape.
  distance :: RealFloat d => a -> Point d -> d
  -- | Get the signed distance function from a shape, with a squared
  -- absolute value.
  distanceSq :: RealFloat d => a -> Point d -> d
  -- | Get the signed distance function from a shape, with a squared
  -- absolute value, using Doubles.
  distanceSqDouble :: a -> Point Double -> Double

  distance s p
    | d < 0 = -sqrt (-d)
    | otherwise = sqrt d
    where d = distanceSq s p
  distanceSq a = realToFrac . distanceSqDouble a . fmap realToFrac
  distanceSqDouble s p 
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
  { shapeDistance :: forall d. RealFloat d => Point d -> d
  , shapeColour :: forall d. RealFloat d => Point d -> Color
  }

instance IsShape Shape where
  distance Shape{shapeDistance} = shapeDistance
  colour Shape{shapeColour} = shapeColour
