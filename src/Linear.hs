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
matMul :: Num dataType
    => Matrix dataType
    -> Matrix dataType
    -> Matrix dataType
matMul matA matB =
    map(\row -> map(\col -> sum(zipWith(*) row col)) (matTrans matB)) matA

{-
    Multiplies a matrix by a scalar.
-}
matScale :: Num dataType
    =>        dataType
    -> Matrix dataType
    -> Matrix dataType
matScale scalar mat =
    map(map(scalar*)) mat

{-
    Adds two matrices together.
-}
matSum :: Num dataType
    => Matrix dataType
    -> Matrix dataType
    -> Matrix dataType
matSum matA matB =
    zipWith(zipWith(+)) matA matB

{-
    Transposes a matrix.
-}
matTrans :: Num dataType
    => Matrix dataType
    -> Matrix dataType
matTrans mat = 
    foldr(zipWith(:)) (repeat []) mat

{-
    Multiplies a matrix by a vector.
-}
matVecMul :: Num dataType
    => Matrix dataType
    -> Vector dataType
    -> Vector dataType
matVecMul mat vec =
    map(\row -> sum(zipWith(*) row vec)) mat

{-
    Adds two vectors together.
-}
vecSum :: Num dataType
    => Vector dataType
    -> Vector dataType
    -> Vector dataType
vecSum vecA vecB =
    zipWith(+) vecA vecB
