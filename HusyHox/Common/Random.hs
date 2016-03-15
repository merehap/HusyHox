module HusyHox.Common.Random (selectOne, selectN) where

import Control.Arrow (first)
import System.Random (randomR, StdGen)

selectOne :: [e] -> StdGen -> (e, StdGen)
selectOne values gen = (values !! index, ngen)
    where (index, ngen) = randomR (0, length values - 1) gen

selectN :: [e] -> Int -> StdGen -> ([e], StdGen)
selectN values n gen = scanState n gen (selectOne values)

scanState :: Int -> s -> (s -> (a, s)) -> ([a], s)
scanState 0 s _ = ([], s)
scanState n s f = first (a :) (scanState (n - 1) ns f)
    where (a, ns) = f s
