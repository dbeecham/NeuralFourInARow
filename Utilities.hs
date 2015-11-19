module Utilities ( map2
                 , partitionEvery
                 , partition
                 , add
                 , pairs
                 , removeLast
                 , car
                 , cdr
                 , cadr
                 , (&)
                 , nrandoms
                 , pick
                 )
    where


import System.Random

map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 f as bs = map (uncurry f) (zip as bs)

(&) = flip ($)

partitionEvery :: Int -> [a] -> [[a]]
partitionEvery _ [] = []
partitionEvery size xs = (take size xs) : (partitionEvery size (drop size xs))


partition :: [Int] -> [a] -> [[a]]
partition _ [] = []
partition [] _ = []
partition (l:lengths) xs = (take l xs):(partition lengths (drop l xs))

add :: (a -> b) -> [a] -> [(b, a)]
add f xs = zip (map f xs) xs

pairs :: [a] -> [(a, a)]
pairs xs = zip xs (drop 1 xs)

removeLast :: Int -> [a] -> [a]
removeLast n xs = take ((length xs) - n) xs

car = head
cdr = tail
cadr = car . cdr

nrandoms :: (Random a, RandomGen g) => g -> Int -> ([a], g)
nrandoms g 0 = ([], g)
nrandoms g n = 
    let (a, g') = random g
        (rest, g'') = nrandoms g' (n-1)
    in (a:rest, g'')


-- Takes a list of (probability, value) and picks out a value at random.
pick :: RandomGen g => g -> [(Double, a)] -> a
pick g vs = undefined
