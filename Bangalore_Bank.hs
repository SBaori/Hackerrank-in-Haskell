import qualified Data.Map as M
import Data.List (minimumBy)

keysMap :: [Int]
keysMap = [9, 0, 1, 2, 3, 4, 5, 6, 7, 8]

solve :: [Int] -> Int
solve (n1:ns) = snd $ minimumBy (\(_, d1) (_, d2) -> compare d1 d2) $ last table
    where
        table :: [[((Int,Int), Int)]]
        table = initCell : [cell | cell <- zipWith nextCell ns table]
            where
            initCell = [((n1, n2), 1) | n2 <- ns]
            
            nextCell :: Int -> [((Int,Int), Int)] -> [((Int,Int), Int)]
            nextCell num pc = M.toList 
                              $ M.fromListWith min 
                              $ concat 
                              $ map (\((n1, n2), d) -> ((num, n2), 1 + d + abs ((keysMap !! num) - (keysMap !! n1))):((n1, num), 1 + d + abs ((keys !! num) - (keys !! n2))):[]) pc
  

main :: IO ()
main = do
  _ <- getLine
  nums <- getLine
  let nums' = map read $ words nums
  print $ solve nums'
