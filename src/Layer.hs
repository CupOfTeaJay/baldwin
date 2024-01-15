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
    init :: Int
        -> Int
        -> (Float -> Float)
        -> Int
        -> layer
    activate :: Linear.Vector Float
        -> layer
        -> layer
