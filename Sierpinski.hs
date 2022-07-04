import Data.List

fst' :: (a,a,a) -> a
fst' (a,_,_) = a

snd' :: (a,a,a) -> a
snd' (_,a,_) = a

trd' :: (a,a,a) -> a
trd' (_,_,a) = a

-- Generates triangle row-wise
generateTriaPos :: [(Int,Int,Int)] -> [(Int,Int,Int)]
generateTriaPos (p:ps)
    | (snd' p) /= (trd' p) = [head new] ++ generateTriaPos (tail new)
    | otherwise = (p:ps)
    where
        new = (p:ps) ++ [((1 + (fst' p)),(1 + (snd' p)),((trd' p) - 1))]

-- Expands tuples into increasing lists of common difference 1.
expandTup :: [(Int,Int,Int)] -> [Int]
expandTup [] = []
expandTup (p:ps) = [nums | nums <- [snd' p .. trd' p]] ++ expandTup ps

-- Generates row with background char '_' and triangle char '1'
generateRow :: Int -> Char -> [Int] -> [Char]
generateRow n bg pos
    | (n < 0) = []
    | (pos /= []) && (n == head pos) = ['1'] ++ generateRow (n-1) bg (tail pos)
    | otherwise = [bg] ++ generateRow (n-1) bg pos

-- (The sierpinski function!) Generates the start and end position pos of the triangle at every depth (DFS implementation).
solve :: Int -> Int -> Int -> Int -> [(Int,Int,Int)]
solve r s e 0 = [(r,s,e)]
solve r s e n = (solve r s e' (n-1)) -- bottom left tri 
                ++ (solve r s' e (n-1)) -- bottom right tri
                ++ (solve (r + (div dist 4) + 1) (s + (div dist 4) + 1) (e - (div dist 4) - 1) (n-1)) -- top mid tri
     where 
     dist = e-s+1
     s' = s + (div (dist) 2) + 1
     e' = s + (div (dist) 2) - 1


main = interact $ unlines 
                . reverse 
                . map (generateRow 62 '_' . reverse . expandTup) 
                . groupBy (\x y -> (fst' x) == (fst' y)) 
                . sortBy (\x y -> compare (fst' x) (fst' y)) 
                . generateTriaPos 
                . solve 0 0 62 
                . read . head . words

