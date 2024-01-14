{-
    LayerTypes.hs

    This module defines the types of layers that can be concatenated to form a
    neural network. Each layer type is an instance of the "Layer" typeclass.
-}

module LayerTypes where

import qualified Common
import qualified Linear
import qualified Layer

{-
    Helper function for "Layer" types. Calculates the activation for a layer 
    prior to being "squished" by some activation function.
-}
zed :: Linear.Vector Float -- Activation of previous layer.
    -> Linear.Matrix Float -- Weights for current layer.
    -> Linear.Vector Float -- Biases for current layer.
    -> Linear.Vector Float -- Activation of current layer.
zed ingress weights biases =
    Linear.vecSum (Linear.matVecMul weights ingress) biases

{-
    A Dense Layer. Neurons in a layer of this type are connected to every
    ingress feature.
-}
data DenseLayer = DenseLayer {weights :: Linear.Matrix Float, -- Weights matrix for this layer.
                              biases  :: Linear.Vector Float, -- Biases vector for this layer.
                              actFunc :: Float -> Float}      -- Activation function for this layer.
instance Layer.Layer DenseLayer where
    initialize ingress egress activation seed =
        DenseLayer {weights = Common.randMat egress ingress seed,
                    biases  = replicate egress 0,
                    actFunc = activation}
    forwardProp (DenseLayer weights biases actFunc) ingress =
        map actFunc (zed ingress weights biases)
