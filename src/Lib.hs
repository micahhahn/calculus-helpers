module Lib
    ( someFunc
    ) where

import Data.Sparse.SpMatrix
import Data.Sparse.SpVector
import Numeric.LinearAlgebra.Sparse

a :: SpMatrix Double
a = fromListDenseSM 2 [ 1, 3
                      , 1, 1]

b :: SpVector Double
b = mkSpVR 2 [1, 3]

data Exp a = Add (Exp a) (Exp a)
           | Mul (Exp a) (Exp a)
           | Var Char
           | Lit a
           deriving (Show)

simplify :: (Num a) => Exp a -> Exp a
simplify (Mul (Con a) (Con b)) = Con (a * b) 
simplify x = x

x = (Var 'a' `Mul` Var 'a') `Mul` Con 1.0

someFunc :: IO ()
someFunc = do
    putStrLn (show a)
    putStrLn (show b)
