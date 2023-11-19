import qualified Data.Map as M 
import Data.Maybe (fromJust)

getBarStates :: [Int] -> [(Int,Int,Int)]
getBarStates [r1,r2,r3] = filter (\(x,y,z) -> not $ y==0 && z>0) [(s1,s2,s3) | s1 <- [1..r1], s2 <- [0..r2], s3 <- [0..r3]]

updateBar :: (Int,Int,Int) -> (Int,Int) -> (Int,Int,Int)
updateBar (r1,r2,r3) (r,c) = updatedBar
    where
        updatedBar = case r of
            1 -> if r1 >= c && r2 >= c && r3 >= c
                    then (c-1,c-1,c-1)
                    else if r1 >= c && r2 >= c
                        then (c-1,c-1,r3)
                        else if r1 >= c
                            then (c-1,r2,r3)
                            else (r1,r2,r3)
            2 -> if r2 >= c && r3 >= c
                    then (r1,c-1, c-1)
                    else if r2 >= c
                        then (r1,c-1,r3)
                        else (r1,r2,r3)
            3 -> if r3 >= c then (r1,r2,c-1) else (r1,r2,r3)
            _ -> (r1,r2,r3)


helper :: (Int,Int,Int) -> M.Map (Int,Int,Int) Bool -> Bool
helper bar@(r1,r2,r3) hash = foldl (\res c -> res || not (fromJust (M.lookup (updateBar bar c) hash))) False coords 
    where
        coords = [(3,n) | n <- [r3,r3-1..1]] ++ [(2,n) | n <- [r2,r2-1..1]] ++ [(1,n) | n <- [r1,r1-1..2]]


play :: [(Int,Int,Int)] -> M.Map (Int,Int,Int) Bool
play barStates = foldl (\newHash bar -> M.insert bar (helper bar newHash) newHash) hash barStates
    where
        hash = M.fromList (map (\x -> (x,False)) barStates)

gameRes :: [Int] -> String
gameRes bar@[r1,r2,r3] = if fromJust $ M.lookup (r1,r2,r3) (play (getBarStates bar)) then 
                            "WIN"
                         else 
                            "LOSE"
main :: IO ()
main = do
    n <- getLine
    go $ read n
        where
            go 0 = return ()
            go n = do
                s <- getLine
                putStrLn $ gameRes $ map (read :: String -> Int) $ words s
                go (n-1)
