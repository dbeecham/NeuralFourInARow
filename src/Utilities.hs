{-# LANGUAGE OverloadedLists #-}
module Utilities ( map2
                 , partitionEvery
                 , partition'
                 , add
                 , pairs
                 , removeLast
                 , car
                 , cdr
                 , cadr
                 , (&)
                 , nrandoms
                 , pick
                 , pickn
                 , flattentuples
                 , pairs2
                 , changeito
                 )
    where


import System.Random
import Data.List

map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 f as bs = map (uncurry f) (zip as bs)

(&) = flip ($)
(~>) = flip (.)

partitionEvery :: Int -> [a] -> [[a]]
partitionEvery _ [] = []
partitionEvery size xs = (take size xs) : (partitionEvery size (drop size xs))


partition' :: [Int] -> [a] -> [[a]]
partition' _ [] = []
partition' [] _ = []
partition' (l:lengths) xs = (take l xs):(partition' lengths (drop l xs))

add :: (a -> b) -> [a] -> [(b, a)]
add f xs = zip (map f xs) xs

pairs :: [a] -> [(a, a)]
pairs xs = zip xs (drop 1 xs)

pairs2 :: [a] -> [(a, a)]
pairs2 (x:y:xs) = (x,y):(pairs2 xs)
pairs2 _ = []

roundToN :: Int -> Double -> Double
roundToN n v = (v * (10^n))
             & round
             & fromInteger
             & (/ 10^n)

changeito :: [a] -> Int -> a -> [a]
changeito xs i v = (take i xs) ++ [v] ++ (drop (i + 1) xs)

flattentuples :: [(a, a)] -> [a]
flattentuples [] = []
flattentuples ((x, y):xs) = x:y:(flattentuples xs)

removeLast :: Int -> [a] -> [a]
removeLast n xs = take ((length xs) - n) xs

mapfst :: (a -> b) -> (a, c) -> (b, c)
mapfst f (x, y) = (f x, y)

car = head
cdr = tail
cadr = car . cdr

nrandoms :: (Random a, RandomGen g) => g -> Int -> ([a], g)
nrandoms g 0 = ([], g)
nrandoms g n = 
    let (a, g') = random g
        (rest, g'') = nrandoms g' (n-1)
    in (a:rest, g'')

cumulativeSum :: Num a => [a] -> [a]
cumulativeSum = cumulativeSum' 0 

cumulativeSum' :: Num a => a -> [a] -> [a]
cumulativeSum' s [] = []
cumulativeSum' s (x:xs) = (x+s) : cumulativeSum' (x+s) xs

-- Takes a list of (probability, value) and picks out a value at random.
pick :: RandomGen g => g -> [(Double, a)] -> (a, g)
pick _ [] = error "List empty."
pick g vs = 
    let (x, g') = random g
        cumul = zip (map snd vs) (cumulativeSum (map fst vs))
        value = find (\(v, c) -> x < c) cumul
    in case value of
        Just (v, _) -> (v, g')
        Nothing -> (last (map snd vs), g')


pickn :: RandomGen g => g -> [(Double, a)] -> Int -> ([a], g)
pickn _ [] _ = error "List empty."
pickn g _ 0 = ([], g)
pickn g vs n =
    let (v, g') = pick g vs
        (rest, g'') = pickn g' vs (n-1)
    in (v:rest, g'')
