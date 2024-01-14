{-
    ActFuncs.hs

    This module defines activation functions for use in neural networks.
-}

module ActFuncs where

import qualified Linear

{-
    TODO: document.
-}
binaryStep :: (Floating dataType, Ord dataType)
    => dataType
    -> dataType
binaryStep x
    | x < 0     = 0
    | otherwise = 1

{-
    TODO: document.
-}
sigmoid :: Floating dataType
    => dataType
    -> dataType
sigmoid x = 
    1 / (1 + exp(-x))

{-
    TODO: document.
-}
gaussian :: Floating dataType
    => dataType
    -> dataType
gaussian x =
    exp(-x^2)