{-# LANGUAGE GADTs #-}

{-
    Layer.hs

    This module defines the "Layer" typeclass.
-}

module Layer where

import qualified Linear

{-
    TODO: document.
-}
data Layer where
    Dense :: DenseLayer -> Layer

{-
    TODO: document.
-}
data DenseLayer = DenseLayer {
    weights :: Linear.Matrix Double,
    biases  :: Linear.Vector Double,
    actFunc :: Double -> Double
}

{-
    Generic function for activating a layer. Should call the appropriate
    subsequent function based on the type of the layer.
-}
activateLayer ::
    Linear.Vector Double ->
    Layer ->
    Linear.Vector Double
activateLayer ingress (Dense layer) =
    activateDense ingress layer

{-
    Returns the activation of a DenseLayer given some input vector. "Wrapped" by
    activateLayer.
-}
activateDense ::
    Linear.Vector Double ->
    DenseLayer ->
    Linear.Vector Double
activateDense ingress layer =
    map (actFunc layer) $ Linear.vecSum (Linear.matVecMul (weights layer) ingress) (biases layer)

{-
    Constructor-like function for a DenseLayer.
-}
initDense ::
    Int ->                -- Number of inputs
    Int ->                -- Number of outputs
    (Double -> Double) -> -- Activation function
    Int ->                -- Seed
    DenseLayer
initDense inShape outShape func seed =
    DenseLayer {
        weights = Linear.linearTransformMat (Linear.randMat outShape inShape seed) (2) (-1),
        biases  = replicate outShape 0,
        actFunc = func
    }
