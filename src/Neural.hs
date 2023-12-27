module Neural where

import Linear

data DenseLayer dataType = DenseLayer {weights :: Linear.Matrix dataType, 
                                       biases :: Linear.Vector dataType}

calcLayer :: Num dataType =>
    Linear.Vector dataType ->
    DenseLayer dataType ->
    Linear.Vector dataType
calcLayer ingress layer =
    Linear.vecSum (Linear.matVecMul (weights layer) ingress) (biases layer)
