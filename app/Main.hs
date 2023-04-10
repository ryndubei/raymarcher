{-# LANGUAGE NamedFieldPuns #-}
module Main (main) where

import Graphics.Gloss.Raster.Field
import World
import World.Shape (Shape)
import Raymarch (runRaymarcher, Config(..))
import qualified Linear as L
import Graphics.Gloss.Interface.Pure.Game (Event (EventKey), Key (..), KeyState (..), SpecialKey (..))
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

windowWidth, windowHeight :: Int
windowWidth = 600
windowHeight = 600

window :: Display
window = InWindow "Raymarcher" (windowWidth, windowHeight) (0, 0)

data CameraState = CameraState
  { cameraPosition :: L.V3 Double
  , cameraRotation :: Double
  , cameraShape :: Shape
  , cameraSun :: Double
  , pressedKeys :: S.Set Key
  }

initialCameraState :: CameraState
initialCameraState = CameraState
  { cameraPosition = L.V3 0 0 0
  , cameraRotation = 0
  , cameraShape = yellowSphereOnPlaneGreenCube
  , cameraSun = 0
  , pressedKeys = S.empty
  }

cameraDirection :: CameraState -> L.V3 Double
cameraDirection CameraState{cameraRotation} = L.rotate (L.axisAngle (L.V3 0 1 0) cameraRotation) (L.V3 0 0 1)

main :: IO ()
main =
  playField window (7,7) 15 initialCameraState getColourAtPoint handleKeys update

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
  , ambientLighting = 0.1
  }

handleKeys :: Event -> CameraState -> CameraState
handleKeys (EventKey key Down _ _) state = state { pressedKeys = S.insert key (pressedKeys state) }
handleKeys (EventKey key Up _ _) state = state { pressedKeys = S.delete key (pressedKeys state) }
handleKeys _ state = state

movementKeybindings :: M.Map Key (CameraState -> CameraState)
movementKeybindings = M.fromList [(Char 'w', \state -> state { cameraPosition = cameraPosition state + cameraDirection state L.^* 0.5 })
                                 ,(Char 's', \state -> state { cameraPosition = cameraPosition state - cameraDirection state L.^* 0.5 })
                                 ,(Char 'a', \state -> state { cameraPosition = cameraPosition state - L.cross (L.V3 0 1 0) (cameraDirection state) L.^* 0.5 })
                                 ,(Char 'd', \state -> state { cameraPosition = cameraPosition state + L.cross (L.V3 0 1 0) (cameraDirection state) L.^* 0.5 })
                                 ,(Char 'r', \state -> state { cameraPosition = cameraPosition state + L.V3 0 0.5 0 })
                                 ,(Char 'f', \state -> state { cameraPosition = cameraPosition state - L.V3 0 0.5 0 })
                                 ,(Char 'h', \state -> state { cameraRotation = cameraRotation state - 0.1 })
                                 ,(Char 'l', \state -> state { cameraRotation = cameraRotation state + 0.1 })
                                 ,(SpecialKey KeyLeft, \state -> state { cameraSun = cameraSun state + 0.1 })
                                 ,(SpecialKey KeyRight, \state -> state { cameraSun = cameraSun state - 0.1 })
                                 ]

update :: Float -> CameraState -> CameraState
update _ = handleMove

handleMove :: CameraState -> CameraState
handleMove state@CameraState{pressedKeys} =
  let f = S.foldr (\key f' -> fromMaybe id (M.lookup key movementKeybindings) . f') id pressedKeys
   in f state