module Linear where

type Matrix dataType = [[dataType]]
type Vector dataType = [dataType]

matSum :: Num dataType =>
    Matrix dataType ->
    Matrix dataType ->
    Matrix dataType
matSum matA matB = 
    zipWith(zipWith (+)) matA matB

matMul :: Num dataType =>
    Matrix dataType ->
    Matrix dataType ->
    Matrix dataType
matMul matA matB = 
    map(\row -> map(\col -> sum(zipWith(*) row col)) (matTrans matB)) matA

matScale :: Num dataType =>
           dataType ->
    Matrix dataType ->
    Matrix dataType
matScale scalar mat = 
    map(map(scalar*)) mat
    
matTrans :: Num dataType =>
    Matrix dataType ->
    Matrix dataType
matTrans mat = 
    foldr(zipWith(:)) (repeat []) mat
