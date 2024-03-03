{-
    ActFuncs.hs

    This module defines activation functions for use in neural networks.
-}

module ActFuncs where

{-
    Heavystep / Binary activation function. Returns "0" when input is
    negative, otherwise returns "1".
-}
binaryStep :: 
    (Num a, Ord a) =>
    a ->
    a
binaryStep x
    | x < 0     = 0
    | otherwise = 1

{-
    Logistic / Sigmoid / Soft Step activation function.
-}
sigmoid :: 
    (Num a, Floating a) =>
    a ->
    a
sigmoid x = 
    1 / (1 + exp(-x))

{-
    Gaussian activation function.
-}
gaussian :: 
    (Num a, Floating a) =>
    a ->
    a
gaussian x =
    exp(-x^2)

