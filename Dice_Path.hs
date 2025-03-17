import Data.List (maximumBy)
import qualified Data.Map as M

initDie :: [Int]
initDie = [1,6,5,2,3,4]

rotateDice :: [Int] -> Bool -> [Int]
rotateDice (f:b:t:bm:l:r:[]) True = (l:r:t:bm:b:f:[])
rotateDice (f:b:t:bm:l:r:[]) False = (t:bm:b:f:l:r:[])

table :: [[[([Int], Int)]]]
table = initRow : (nextRow <$> table)
    where
        initRow = iterate (\[(d,v)] -> let rd@(f:_) = rotateDice d True in [(rd, v+f)]) [(initDie,1)]

        nextRow :: [[([Int], Int)]] -> [[([Int], Int)]]
        nextRow pr@([pc]:_) = let r = initCell : zipWith nextCell (tail pr) r in r
            where
                (d,v) = pc
                initCell = let rd@(f:_) = rotateDice d False in [(rd, v+f)]
        
        nextCell :: [([Int],Int)] -> [([Int],Int)] -> [([Int],Int)]
        nextCell pc cc = M.toList $ M.fromListWith max (pc' ++ cc')
            where
                pc' = map (\(d,v) -> let rd@(f:_) = rotateDice d False in (rd, v+f)) pc
                cc' = map (\(d,v) -> let rd@(f:_) = rotateDice d True in (rd, v+f)) cc

main :: IO ()
main = do
    n <- getLine 
    go $ read n
    where
        go 0 = pure ()
        go i = do
            s <- getLine
            let [n,m] = map read $ words s
            print $ snd $ maximumBy (\(_,v1) (_,v2) -> compare v1 v2) (table !! (n-1) !! (m-1))
            go $ (i-1)
