module Neural where

import Linear

data Layer = DenseLayer {weights :: Linear.Matrix Float, 
                         biases :: Linear.Vector Float}
