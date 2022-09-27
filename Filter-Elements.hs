import Data.List

getElem :: Int -> [[Int]] -> [Int]
getElem _ [] = []
getElem k x = val ++ getElem k (tail x)
    where
        val = if ((length $ head x) >= k) then [head $ head x] else []

firstOcc :: [Int] -> [Int] -> [Int]
firstOcc _ [] = [-1]
firstOcc org x = nub $ filter (\y -> elem y x) org

solve :: (Int,[Int]) -> [Int]
solve x = let s = getElem (fst x) $ group $ sort (snd x) 
          in firstOcc (snd x) s


parse :: [Int] -> [(Int,[Int])]
parse [] = []
parse inp = [(head $ tail inp, fst $ tc)] ++ parse (snd tc)
    where
        tc = splitAt (head inp) (snd $ splitAt 2 inp)

intListToString :: [Int] -> String
intListToString sols = foldr (\x y -> (show x) ++ " " ++ (y)) "" sols

main = interact $ unlines . map (intListToString . solve) . parse . map (read :: String -> Int) . tail . words
