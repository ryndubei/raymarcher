{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RebindableSyntax #-}
module World.Shape (Shape, IsShape(..), toShape, Point, normal, intersection, difference) where

import Prelude hiding ((>), (<), (&&), (||), (==), not, otherwise)
import qualified Linear as L
import qualified Data.Array.Accelerate.Linear ()
import qualified Data.Array.Accelerate.Linear as AL
import Graphics.Gloss.Accelerate.Raster.Field (Colour)
import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate (ifThenElse, (<), (>))

-- | 3-dimensional vector. 
type Point = L.V3 Double

class IsShape a where
  -- | Get the colour of the nearest point on a shape.
  colour :: a -> A.Exp Point -> A.Exp Colour
  -- | Get the signed distance function from a shape.
  distance :: a -> A.Exp Point -> A.Exp Double

-- | Approximate the unit normal vector to the nearest surface point
-- on a shape for a given epsilon value.
normal :: IsShape a => A.Exp Double -> a -> A.Exp Point -> A.Exp Point
normal eps s x = 
  let v1 = L.V3 (distance s $ x + A.lift (L.V3 eps 0 0)) (distance s $ x + A.lift (L.V3 0 eps 0)) (distance s $ x + A.lift (L.V3 0 0 eps))
      v2 = L.V3 (distance s $ x - A.lift (L.V3 eps 0 0)) (distance s $ x - A.lift (L.V3 0 eps 0)) (distance s $ x - A.lift (L.V3 0 0 eps))
   in AL.normalize (A.lift $ v1 - v2)

-- | Conversion to the generic shape type, which allows us to do
-- transformations on the distance function.
toShape :: IsShape a => a -> Shape
toShape s = Shape (distance s) (colour s)

-- | A Shape is something that possesses a distance function and
-- a colour.
data Shape = Shape 
  { shapeDistance :: A.Exp Point -> A.Exp Double
  , shapeColour :: A.Exp Point -> A.Exp Colour
  }

instance IsShape Shape where
  distance Shape{shapeDistance} = shapeDistance
  colour Shape{shapeColour} = shapeColour

instance Semigroup Shape where
  s1 <> s2 = Shape d c 
    where
      d x = min (shapeDistance s1 x) (shapeDistance s2 x)
      c x = if shapeDistance s1 x < shapeDistance s2 x 
        then shapeColour s1 x
        else shapeColour s2 x

intersection :: Shape -> Shape -> Shape
intersection s1 s2 = Shape d c 
  where
    d x = max (shapeDistance s1 x) (shapeDistance s2 x)
    c x = if shapeDistance s1 x > shapeDistance s2 x
      then shapeColour s1 x
      else shapeColour s2 x

difference :: Shape -> Shape -> Shape
difference s1 s2 = Shape d c 
  where
    d x = max (shapeDistance s1 x) (- shapeDistance s2 x)
    c x = if shapeDistance s1 x > - shapeDistance s2 x 
      then shapeColour s1 x
      else shapeColour s2 x