module World where

import World.Shape
import World.Shapes
import Graphics.Gloss.Data.Color (greyN, yellow)
import qualified Linear as L

yellowSphere :: Shape
yellowSphere = toShape (Sphere 1 (L.V3 0 0 4) yellow)

yellowSphereOnPlane :: Shape
yellowSphereOnPlane = toShape (Sphere 1 (L.V3 0 0 4) yellow) <> toShape (Plane (L.V3 0 1 0) (-1.5) (greyN 0.5))