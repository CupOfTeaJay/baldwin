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
    -> Linear.Vector Float -- "Raw" activation of current layer.
zed ingress weights biases =
    Linear.vecSum (Linear.matVecMul weights ingress) biases

{-
    A Dense Layer. Neurons in a layer of this type are connected to every
    ingress feature.
-}
data DenseLayer = DenseLayer {weights :: Linear.Matrix Float,
                              biases  :: Linear.Vector Float,
                              actFunc :: Float -> Float,
                              preds   :: Linear.Vector Float}
instance Layer.Layer DenseLayer where
    init ingress egress function seed =
        DenseLayer {weights = Linear.randMat egress ingress seed,
                    biases  = replicate egress 0,
                    actFunc = function,
                    preds   = replicate egress 0}
    activate ingress layer =
        DenseLayer {weights = weights layer,
                    biases  = biases layer,
                    actFunc = actFunc layer,
                    preds   = map (actFunc layer) $ zed ingress (weights layer) (biases layer)}
