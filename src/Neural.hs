{-
    Neural.hs

    This module contains types and functions for constructing and training
    neural networks.
-}

module Neural where

import Linear

{-
    TODO: document.
-}
data DenseLayer dataType = DenseLayer {weights :: Linear.Matrix dataType,
                                       biases :: Linear.Vector dataType}

{-
    TODO: document.
-}
data FullyConnected dataType = FullyConnected {layers :: [DenseLayer dataType]}

---------- ACTIVATION FUNCTIONS ----------

binaryStep :: (Num dataType, Ord dataType)
    => dataType
    -> dataType
binaryStep x
    | x < 0     = 0
    | otherwise = 1

sigmoid :: Floating dataType
    => dataType
    -> dataType
sigmoid x = 
    1 / (1 + exp(-x))

gaussian :: Floating dataType
    => dataType
    -> dataType
gaussian x =
    exp(-x^2)

{-
    TODO: document.
-}
calcLayer :: Num dataType
    => Linear.Vector dataType
    -> DenseLayer    dataType
    -> Linear.Vector dataType
calcLayer ingress layer =
    Linear.vecSum (Linear.matVecMul (weights layer) ingress) (biases layer)
