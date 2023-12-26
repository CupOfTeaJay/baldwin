module Neural where

import Linear

data DenseLayer = DenseLayer {weights :: Linear.Matrix Float, 
                              biases :: Linear.Vector Float}

calcLayer ::
    Linear.Vector Float ->
    DenseLayer ->
    Linear.Vector Float
calcLayer ingress layer =
    Linear.vecSum (Linear.matVecMul (weights layer) ingress) (biases layer)
