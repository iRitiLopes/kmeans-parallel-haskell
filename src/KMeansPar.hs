module KMeansPar where

import Data.List
import Data.Function
import Control.Parallel.Strategies
import Data.Array
import Control.DeepSeq

parseFile::String -> [[Double]]
parseFile file = map (\xs -> toDouble $ words xs) $ lines file
  where
    toDouble [] = []
    toDouble (x:xs) = (read x :: Double) : toDouble xs

(./) :: (Floating a, Functor f) => f a -> a -> f a
xs ./ x = (/x) <$> xs

(.^) :: (Floating a, Functor f) => f a -> Int -> f a
xs .^ x = (^x) <$> xs
