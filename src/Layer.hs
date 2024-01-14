{-
    Layer.hs

    This module defines the "Layer" typeclass.
-}

module Layer where

import qualified Linear

{-
    TODO: document.
-}
class Layer layer where
    initialize :: Int
        -> Int
        -> (Float -> Float)
        -> Int
        -> layer
    forwardProp :: layer
        -> Linear.Vector Float
        -> Linear.Vector Float
