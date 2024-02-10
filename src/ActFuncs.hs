{-
    ActFuncs.hs

    This module defines activation functions for use in neural networks.
-}

module ActFuncs where

{-
    TODO: document.
-}
binaryStep :: 
    (Num a, Ord a) =>
    a ->
    a
binaryStep x
    | x < 0     = 0
    | otherwise = 1

{-
    TODO: document.
-}
sigmoid :: 
    (Num a, Floating a) =>
    a ->
    a
sigmoid x = 
    1 / (1 + exp(-x))

{-
    TODO: document.
-}
gaussian :: 
    (Num a, Floating a) =>
    a ->
    a
gaussian x =
    exp(-x^2)

