module World where

import World.Shape
import World.Shapes
import Graphics.Gloss.Data.Color (greyN, yellow)

yellowSphere :: Shape
yellowSphere = toShape (Sphere 1 (0, 0, 4) yellow)

yellowSphereOnPlane :: Shape
yellowSphereOnPlane = toShape (Sphere 1 (0, 0, 4) yellow) <> toShape (Plane (0, 1, 0) (-1.5) (greyN 0.5))