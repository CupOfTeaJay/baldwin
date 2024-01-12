{-
    Layer.hs

    TODO: document.
-}

module Layer where

import qualified Linear

{-
    TODO: document.
-}
class Layer x where
    
    init        :: Int -> Int -> (Float -> Float) -> x
    forwardProp :: x -> Linear.Vector Float -> Linear.Vector Float

{-
    TODO: document.
-}
data DenseLayer = DenseLayer {weights :: Linear.Matrix Float,
                              biases  :: Linear.Vector Float,
                              actFunc :: Float -> Float}

instance Layer DenseLayer where
    init ingress egress activation =
        DenseLayer {weights = [[]], -- TODO: how do we initialize this in a random fashion?
                    biases  = replicate egress 0,
                    actFunc = activation}

    forwardProp (DenseLayer weights biases actFunc) ingress =
        map actFunc (Linear.vecSum (Linear.matVecMul weights ingress) biases)
