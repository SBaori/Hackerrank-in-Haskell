import qualified Data.Vector as V
import qualified Data.Set as S
import Data.List (group, foldl1)
import Data.Bits (xor)
import Data.Char (isNumber)

mex :: [Int] -> Int
mex xs = go 0 (S.fromList $ filter (>= 0) xs)
  where
    go n s
      | n `S.member` s = go (n + 1) s
      | otherwise = n

grundy :: V.Vector Int
grundy = V.generate 300 getGrundyVal
    where
        getGrundyVal :: Int -> Int
        getGrundyVal 0 = 0
        getGrundyVal n = mex
            $ map (\(x,y) -> (grundy V.! x) `xor` (grundy V.! y))
            $ [(0 + l, n - l - 1) | l <- [0..n-1]] ++ [(0 + l, n - l - 2) | l <- [0..n-2]]

solve :: [String] -> String
solve = unlines 
    . map ((\val -> if val == 0 then "LOSE" else "WIN") 
        . foldl1 xor 
        . map ((grundy V.!) . length) 
        . filter ((== 'I') . head) 
        . group)

main :: IO ()
main = interact $ solve . filter (not . isNumber . head) . lines
