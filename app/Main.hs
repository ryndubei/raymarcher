{-# LANGUAGE NamedFieldPuns #-}
module Main (main) where

import Graphics.Gloss.Raster.Field
import World
import World.Shape (Shape)
import Raymarch (runRaymarcher, Config(..))
import qualified Linear as L
import Graphics.Gloss.Interface.Pure.Game (Event (EventKey), Key (..), KeyState (..), SpecialKey (..))

windowWidth, windowHeight :: Int
windowWidth = 800
windowHeight = 800

window :: Display
window = InWindow "Raymarcher" (windowWidth, windowHeight) (0, 0)

data CameraState = CameraState
  { cameraPosition :: L.V3 Double
  , cameraRotation :: Double
  , cameraShape :: Shape
  , cameraSun :: Double
  }

initialCameraState :: CameraState
initialCameraState = CameraState
  { cameraPosition = L.V3 0 0 0
  , cameraRotation = 0
  , cameraShape = yellowSphereOnPlaneGreenCube
  , cameraSun = 0
  }

cameraDirection :: CameraState -> L.V3 Double
cameraDirection CameraState{cameraRotation} = L.rotate (L.axisAngle (L.V3 0 1 0) cameraRotation) (L.V3 0 0 1)

main :: IO ()
main = 
  playField window (6,6) 10 initialCameraState getColourAtPoint handleKeys (const id)

getColourAtPoint :: CameraState -> (Float, Float) -> Color
getColourAtPoint CameraState{cameraShape, cameraPosition, cameraRotation, cameraSun} (x, y) = runRaymarcher Config 
  { maxSteps = 100
  , maxDistanceSq = 100
  , epsilon = 0.001
  , initialDirectionVector = L.rotate (L.axisAngle (L.V3 0 1 0) cameraRotation) (L.normalize (L.V3 (realToFrac x) (realToFrac y) 1))
  , initialPositionVector = cameraPosition
  , fog = black
  , sun = L.rotate (L.axisAngle (L.V3 0 0 1) cameraSun) (L.normalize $ L.V3 1 0.5 0)
  , shape = cameraShape
  }

handleKeys :: Event -> CameraState -> CameraState
handleKeys (EventKey (Char 'w') Down _ _) state = state { cameraPosition = cameraPosition state + cameraDirection state }
handleKeys (EventKey (Char 's') Down _ _) state = state { cameraPosition = cameraPosition state - cameraDirection state }
handleKeys (EventKey (Char 'a') Down _ _) state = state { cameraPosition = cameraPosition state - L.cross (L.V3 0 1 0) (cameraDirection state) }
handleKeys (EventKey (Char 'd') Down _ _) state = state { cameraPosition = cameraPosition state + L.cross (L.V3 0 1 0) (cameraDirection state) }
handleKeys (EventKey (Char 'r') Down _ _) state = state { cameraPosition = cameraPosition state + L.V3 0 1 0 }
handleKeys (EventKey (Char 'f') Down _ _) state = state { cameraPosition = cameraPosition state - L.V3 0 1 0 }
handleKeys (EventKey (Char 'h') Down _ _) state = state { cameraRotation = cameraRotation state - 0.1 }
handleKeys (EventKey (Char 'l') Down _ _) state = state { cameraRotation = cameraRotation state + 0.1 }
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) state = state { cameraSun = cameraSun state + 0.1 }
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) state = state { cameraSun = cameraSun state - 0.1 }
handleKeys _ state = state

