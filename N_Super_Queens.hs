import Control.Monad
import Data.Maybe (fromMaybe)
import Control.Applicative ( Alternative((<|>)) ) 

check :: Int -> Int -> [(Int,Int)] -> Bool
check row col sol = not $ any (\(x,y) -> x == row || y == col || 
                                         (abs(x-row) == abs(y-col)) || 
                                         (abs(x-row) <=2 && abs(y-col)<=2)) sol


dfs :: Int -> Int -> [Int] -> [(Int,Int)] -> Maybe [(Int,Int)]
dfs n row cols sol = foldr (\x y -> let helper = guard (check row x sol) >> solve n (row+1) ((row,x):sol) 
                                    in liftM2 (++) helper y <|> helper <|> y) Nothing cols

solve :: Int -> Int -> [(Int,Int)]  -> Maybe [(Int,Int)]
solve n row sol
    | row > n = Just sol
    | otherwise = dfs n row [1..n] sol

nSuperQueens :: Int -> Int
nSuperQueens n = div (length $ fromMaybe [] (solve n 1 [])) n 

main = interact $ show . nSuperQueens . read . head . words
