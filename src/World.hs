module World where

import World.Shape
import World.Shapes
import Graphics.Gloss.Data.Color (greyN, yellow)

yellowSphereOnPlane :: Shape
yellowSphereOnPlane = toShape (Sphere 1 (0, 0, -4) yellow) <> toShape (Plane (0, -2, 0) 0 (greyN 0.5))