import Data.List

fst' :: (a,a,a) -> a
fst' (a,_,_) = a

snd' :: (a,a,a) -> a
snd' (_,a,_) = a

trd' :: (a,a,a) -> a
trd' (_,_,a) = a

cols :: Int
cols = 100

row :: Int
row = 63

-- Expands tuples 
expandTup :: [(Int,Int,Int)] -> [[Int]]
expandTup [] = []
expandTup (p:ps) = [[r+x,c] | x <- [1..(div h 2)]] ++ [[r + (div h 2) + x,c-x,c+x] | x <- [1..(div h 2)]] ++ expandTup ps
    where
    	r = fst' p
        c = snd' p
        h = trd' p

fillGaps :: Int -> [[Char]] -> [[Char]]
fillGaps r p = p ++ [(generateRow (cols - 1) '_' []) | x <- [1..(r - length p)]]

generateRow :: Int -> Char -> [Int] -> [Char]
generateRow n bg pos
    | (n < 0) = []
    | (pos /= []) && (n == head pos) = ['1'] ++ generateRow (n-1) bg (tail pos)
    | otherwise = [bg] ++ generateRow (n-1) bg pos

solve :: Int -> Int -> Int -> Int -> [(Int,Int,Int)]
solve _ _ _ 0 = []
solve r c h n = [(r,c,h)] ++ solve (r + h) (c - h') (h') (n-1) ++ solve (r + h) (c+h') (h') (n-1)
	where
		h' = div h 2

group' :: [[Int]] -> [Int]
group' = foldl (\x y -> x ++ tail y) []

main = interact $ unlines 
		. reverse
		. fillGaps row
		. map (reverse . generateRow (cols - 1) '_' . reverse . group') 
		. groupBy (\x y -> (head x) == (head y)) 
		. sort 
		. expandTup 
		. solve 0 ((div cols 2) - 1) 32 
		. read . head . words 

