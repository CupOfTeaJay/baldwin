module Linear where

type Matrix dataType = [[dataType]]

addMatrices :: Num dataType =>
    Matrix dataType ->
    Matrix dataType ->
    Matrix dataType
addMatrices matrixA matrixB = 
    zipWith(zipWith (+)) matrixA matrixB

scaleMatrix :: Num dataType =>
           dataType ->
    Matrix dataType ->
    Matrix dataType
scaleMatrix scalar matrix =
    map(map(scalar*)) matrix
