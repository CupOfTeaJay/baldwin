module Neural where

import qualified Data.Matrix as Matrix
import qualified Data.Vector as Vector

data Layer = DenseLayer {weights :: Matrix Float, biases :: Vector Float}
