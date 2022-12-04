import Data.Char

intList :: Int -> [Int]
intList 0 = []
intList num = [num `mod` 10] ++ intList (num `div` 10)

super :: Int -> Int
super num = if(num `div` 10 == 0) then num else super $ sum $ intList num

parse :: [String] -> [Int]
parse [n,k] = intList $ (read k)*(foldr (\x y -> digitToInt x + y) 0 n)

main = interact $ show . super . sum . parse . words
