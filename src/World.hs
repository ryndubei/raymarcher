{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module World where

import World.Shape
import World.Shapes
import Graphics.Gloss.Data.Color (greyN, yellow, green)
import qualified Linear as L
import Data.Bool (bool)

yellowSphere :: Shape
yellowSphere = toShape (Sphere 1 (L.V3 0 0 4) yellow)

yellowSphereOnPlane :: Shape
yellowSphereOnPlane = toShape (Sphere 1 (L.V3 0 0 4) yellow) <> toShape (Plane (L.V3 0 1 0) (-1.5) f)
  where
    f (L.V3 x _ z) =
      let x' = floor x `div` 2 :: Integer
          z' = floor z `div` 2 :: Integer
       in bool (greyN 0.3) (greyN 0.7) ((even x' && even z') || (odd x' && odd z'))

yellowSphereOnPlaneGreenCube :: Shape
yellowSphereOnPlaneGreenCube = yellowSphereOnPlane <> toShape (Cube 0.2 (L.V3 1 (-1) 4) green)