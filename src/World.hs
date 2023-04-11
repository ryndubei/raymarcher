{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE FlexibleContexts #-}
module World where

import World.Shape
import World.Shapes
import Data.Array.Accelerate.Data.Colour.Names
import qualified Data.Array.Accelerate as A
import qualified Linear as L

yellowSphere :: Shape
yellowSphere = toShape (Sphere 1 (A.constant $ L.V3 0 0 4) yellow)

tiledPlane :: Shape
tiledPlane = toShape (Plane (A.constant $ L.V3 0 1 0) (-1.5) f)
  where
    f p =
      let (L.V3 x _ z) = A.unlift p :: (L.V3 (A.Exp Double))
          x' = A.floor x `A.div` 2 :: A.Exp Int
          z' = A.floor z `A.div` 2 :: A.Exp Int
       in A.ifThenElse ((A.even x' A.&& A.even z') A.|| (A.odd x' A.&& A.odd z')) lightGrey dimGrey

greenCube :: Shape
greenCube = toShape (Cube 0.2 (A.constant $ L.V3 1 (-1) 4) green)

dice :: Shape
dice = toShape (Cube 1 (A.constant $ L.V3 0 1 4) red) `intersection` toShape (Sphere (sqrt 2) (A.constant $ L.V3 0 1 4) blue)

yellowSphereOnPlane :: Shape
yellowSphereOnPlane = yellowSphere <> tiledPlane

yellowSphereOnPlaneGreenCube :: Shape
yellowSphereOnPlaneGreenCube = yellowSphereOnPlane <> greenCube

diceOnPlane :: Shape
diceOnPlane = dice <> tiledPlane
