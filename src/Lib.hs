{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-} 
{-# LANGUAGE TypeFamilies #-}

module Lib
    ( someFunc
    ) where

import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Data.List (intercalate)
import Data.Sparse.SpMatrix
import Data.Sparse.SpVector
import Numeric.LinearAlgebra.Sparse

a' :: SpMatrix Double
a' = fromListDenseSM 2 [ 1, 3
                      , 1, 1]

b' :: SpVector Double
b' = mkSpVR 2 [1, 3]

data Exp a = Add (Exp a) (Exp a)
           | Mul (Exp a) (Exp a)
           | Var Char
           | Lit a
           deriving (Show, Eq)

instance (Num a) => Num (Exp a) where
    negate = (Lit (-1) `Mul`)
    (+) = Add
    (*) = Mul
    fromInteger = Lit . fromInteger
    abs = id
    signum = id

makeBaseFunctor ''Exp

simp :: (Num a) => Exp a -> Exp a
simp e = cata f e
    where f ((l `Add` r) `MulF` y) = (l * y) + (r * y)
          f (x `MulF` (l `Add` r)) = (x * l) + (x * r)
          f x = embed x

simplify :: (Num a) => Exp a -> Exp a
{- Always distribute multiplications -}
simplify ((Add l r) `Mul` y) = (l * y) + (r * y)
simplify (x `Mul` (Add l r)) = (x * l) + (x * r)
simplify x = x

simplifyR :: (Num a, Eq a) => Exp a -> [Exp a]
simplifyR e = let e' = simp e
               in if e == e' then [e] else e : simplifyR e'

p :: (Show a) => [Exp a] -> String
p es = intercalate "\n" (pretty <$> es)

pretty :: (Show a) => Exp a -> String
pretty (l `Add` r) = pretty l ++ " + " ++ pretty r
pretty (x `Mul` y) = let x' = case x of
                                  (_ `Add` _) -> "(" ++ pretty x ++ ")"
                                  _ -> pretty x
                         y' = case y of 
                                  (_ `Add` _) -> "(" ++ pretty y ++ ")"
                                  _ -> pretty y
                      in x' ++ y'
pretty (Var c) = [c]
pretty (Lit a) = show a

a = Var 'a'
b = Var 'b'
c = Var 'c'
d = Var 'd'
e = Var 'e'
f = Var 'f'

test = ((a + b) * (c + d)) * (e + f)

someFunc :: IO ()
someFunc = do
    putStrLn (show test)
