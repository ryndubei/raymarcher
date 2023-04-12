import Raymarch
import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.Linear as L
import Data.Array.Accelerate.Data.Colour.Names
import World (yellowSphere)
import qualified Data.Array.Accelerate.Interpreter as Interp
import Data.Array.Accelerate.Data.Colour.RGBA (packRGBA)

main :: IO ()
main = do
  let conf = Config 100 100 0.01 (A.lift $ L.V3 (0.0 :: Double) 0.0 1.0) (A.lift $ L.V3 (0.0 :: Double) 0.0 0.0) black (A.lift $ L.V3 1.0 0.5 (0.0::Double)) yellowSphere 0.1
      f = runRaymarcher conf
      arr = A.generate (A.constant A.Z) (const (packRGBA f))
  putStrLn "Running test"
  let d = Interp.run arr
  print (A.indexArray d A.Z)
  putStrLn "Done"

