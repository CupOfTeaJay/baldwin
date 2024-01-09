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

{-
    TODO: document.
-}
calcLayer :: Num dataType
    => Linear.Vector dataType
    -> DenseLayer    dataType
    -> Linear.Vector dataType
calcLayer ingress layer =
    Linear.vecSum (Linear.matVecMul (weights layer) ingress) (biases layer)
