module World where

import Bitfield

data World = World {
    cells :: Bitfield (Int, Int)
  , playing :: Bool
 }

new (width, height) seed = World {
    cells = bitfieldFromInteger (\(x, y) -> y * width + x) seed
  , playing = False
}


