module Linear where

type Vector dataType = [dataType]
type Matrix dataType = [Vector dataType]

matMul :: Num dataType
    => Matrix dataType
    -> Matrix dataType
    -> Matrix dataType
matMul matA matB =
    map(\row -> map(\col -> sum(zipWith(*) row col)) (matTrans matB)) matA

matScale :: Num dataType
    =>        dataType
    -> Matrix dataType
    -> Matrix dataType
matScale scalar mat =
    map(map(scalar*)) mat

matSum :: Num dataType
    => Matrix dataType
    -> Matrix dataType
    -> Matrix dataType
matSum matA matB =
    zipWith(zipWith(+)) matA matB
    
matTrans :: Num dataType
    => Matrix dataType
    -> Matrix dataType
matTrans mat = 
    foldr(zipWith(:)) (repeat []) mat

matVecMul :: Num dataType
    => Matrix dataType
    -> Vector dataType
    -> Vector dataType
matVecMul mat vec =
    map(\row -> sum(zipWith(*) row vec)) mat

vecSum :: Num dataType
    => Vector dataType
    -> Vector dataType
    -> Vector dataType
vecSum vecA vecB =
    zipWith(+) vecA vecB
