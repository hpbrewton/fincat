module Lib
    ( someFunc
    ) where

import Data.List.Unique

data FiniteCatagory l o = FiniteCatagory {
    objects :: [o],
    edges :: o -> [l],
    follow :: o -> l -> o 
}

kkernel :: (Eq l, Ord l) => FiniteCatagory l o -> o -> Int -> [[l]]
kkernel fc _ 0 = [[]]
kkernel fc o d = do 
    edge <- (edges fc) o 
    let next = (follow fc) o edge 
    tpath <- kkernel fc next (d-1)
    let path = edge : tpath
    ex <- [path, tpath]
    return ex

example = FiniteCatagory 
    { objects = [0..1]
    , edges = (\ o -> (case o of 
        0 -> []
        1 -> ["left", "top", "bot", "val", "right"]))
    , follow = (\ o l -> (case (o, l) of 
        (1, "left") -> 1
        (1, "right") -> 1
        (1, "top") -> 1
        (1, "bot") -> 1
        (1, "val") -> 0))
    }

someFunc :: IO ()
someFunc = do 
    let f = kkernel example 1 2
    putStrLn $ show f
