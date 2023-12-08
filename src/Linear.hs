module Linear where

type Matrix dataType = [[dataType]]

addMatrices :: Num dataType => 
    Matrix dataType -> 
    Matrix dataType -> 
    Matrix dataType
addMatrices matrixA matrixB = 
    zipWith(zipWith (+)) matrixA matrixB
