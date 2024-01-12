{-
    Linear.hs

    This module contains types and functions for performing linear algebra
    operations.
-}

module Linear where

{-
    Alias for a list, effectively a vector.
-}
type Vector dataType = [dataType]

{-
    Alias for a list of lists, effectively a matrix.
-}
type Matrix dataType = [Vector dataType]

{-
    Multiplies two matrices together.
-}
matMul :: Floating dataType
    => Matrix dataType
    -> Matrix dataType
    -> Matrix dataType
matMul matA matB =
    map(\row -> map(\col -> sum(zipWith(*) row col)) (matTrans matB)) matA

{-
    Multiplies a matrix by a scalar.
-}
matScale :: Floating dataType
    =>        dataType
    -> Matrix dataType
    -> Matrix dataType
matScale scalar mat =
    map(map(scalar*)) mat

{-
    Adds two matrices together.
-}
matSum :: Floating dataType
    => Matrix dataType
    -> Matrix dataType
    -> Matrix dataType
matSum matA matB =
    zipWith(zipWith(+)) matA matB

{-
    Transposes a matrix.
-}
matTrans :: Floating dataType
    => Matrix dataType
    -> Matrix dataType
matTrans mat = 
    foldr(zipWith(:)) (repeat []) mat

{-
    Multiplies a matrix by a vector.
-}
matVecMul :: Floating dataType
    => Matrix dataType
    -> Vector dataType
    -> Vector dataType
matVecMul mat vec =
    map(\row -> sum(zipWith(*) row vec)) mat

{-
    Dot product of two vectors.
-}
vecDot :: Floating dataType
    => Vector dataType
    -> Vector dataType
    -> dataType
vecDot vecA vecB =
    sum(zipWith(*) vecA vecB)

{-
    Subtracts one vector from another.
-}
vecSub :: Floating dataType
    => Vector dataType
    -> Vector dataType
    -> Vector dataType
vecSub vecA vecB =
    zipWith(-) vecA vecB

{-
    Adds two vectors together.
-}
vecSum :: Floating dataType
    => Vector dataType
    -> Vector dataType
    -> Vector dataType
vecSum vecA vecB =
    zipWith(+) vecA vecB
