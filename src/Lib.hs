{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-} 
{-# LANGUAGE TypeFamilies #-}

module Lib
    ( someFunc
    ) where

import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Data.List (intercalate, partition, foldl1', foldl', sort, sortOn)
import Data.Sparse.SpMatrix
import Data.Sparse.SpVector
import Numeric.LinearAlgebra.Sparse

a' :: SpMatrix Double
a' = fromListDenseSM 2 [ 1, 3
                      , 1, 1]

b' :: SpVector Double
b' = mkSpVR 2 [1, 3]

data Exp a = Add [Exp a]
           | Mul [Exp a]
           | Lit a
           | Var Char
           deriving (Show, Eq, Ord)

instance (Num a) => Num (Exp a) where
    negate x = (Lit (-1) * x)
    (+) x y = Add [x, y]
    (*) x y = Mul [x, y]
    fromInteger = Lit . fromInteger
    abs = id
    signum = id

makeBaseFunctor ''Exp

simp :: (Num a, Ord a) => Exp a -> Exp a
simp e = cata order . cata dist . cata flatten $ e
    where flatten (MulF as) = let (ms, bs) = partition isMul as
                               in Mul $ concat [ms' | (Mul ms') <- ms ] ++ bs
          flatten (AddF as) = let (ms, bs) = partition isAdd as
                               in Add $ concat [ms' | (Add ms') <- ms ] ++ bs
          flatten x = embed x

          dist (MulF as) = foldl1' (\l r -> Add $ (\l' r' -> flatten $ MulF [l', r']) <$> unAdd l <*> unAdd r) as
          dist (AddF as) = flatten (AddF as)
          dist x = embed x

          order (MulF as) = let (ls, ns) = partition isLit as
                             in Mul $ sort ((Lit $ product [ x | (Lit x) <- ls]) : ns)
          order (AddF as) = let adds = sortOn (filter (not . isLit) . unMul) as
                                x = foldl' (\((pl:pns):ps) (al:ans) -> if pns == ans then ((addLit pl al):pns):ps else (al:ans):(pl:pns):ps) [(unMul . head $ adds)] (unMul <$> tail adds)
                             in Add (Mul <$> x)
          order x = embed x

          unAdd (Add as) = as
          unAdd x = [x]

          unMul (Mul as) = as
          unMul x = [x]

          isMul (Mul _) = True
          isMul _ = False

          isAdd (Add _) = True
          isAdd _ = False

          isLit (Lit _) = True
          isLit _ = False

          addLit (Lit l) (Lit r) = Lit (l + r) 

p :: (Show a) => [Exp a] -> String
p es = intercalate "\n" (pretty <$> es)

pretty :: (Show a) => Exp a -> String
pretty (Add as) = intercalate " + " (pretty <$> as)
pretty (Mul ms) = concat $ (\m -> case m of
                                     Add _ -> "(" ++ pretty m ++ ")"
                                     _ -> pretty m) <$> ms
pretty (Var c) = [c]
pretty (Lit a) = show a

a = Var 'a'
b = Var 'b'
c = Var 'c'
d = Var 'd'
e = Var 'e'
f = Var 'f'

test = (a + 1)^5

someFunc :: IO ()
someFunc = do
    putStrLn (show test)
