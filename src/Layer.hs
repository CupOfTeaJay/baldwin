{-
    Layer.hs

    TODO: document.
-}

module Layer where

import qualified ActFuncs
import qualified Linear

{-
    TODO: document.
-}
class Layer x where
    {-
        TODO: document.
    -}
    forwardProp :: x
        -> Linear.Vector Float
        -> Linear.Vector Float

{-
    TODO: document.
-}
data DenseLayer = DenseLayer {weights :: Linear.Matrix Float,
                              biases  :: Linear.Vector Float,
                              actFunc :: Float -> Float}

instance Layer DenseLayer where
    forwardProp (DenseLayer weights biases actFunc) ingress =
        map actFunc (Linear.vecSum (Linear.matVecMul weights ingress) biases)
