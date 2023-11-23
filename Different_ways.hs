import Data.Array

getRes :: Array (Int,Int) Int
getRes = listArray ((0,0),(1000,1000)) . concat $ combinations 
    where
        rowToArray l = listArray (0, length l - 1) l

-- bottom up dp with the concept nCk = (n-1)Ck + (n-1)C(k-1)
combinations :: [[Int]]
combinations = (1:replicate 1000 0) : (nextRow <$> combinations)
  where 
    nextRow l@(_:t) = 1 : (zipWith f l t)
    f x y = (x + y) `mod` (10^8 + 7)

main :: IO ()
main = do
    n <- getLine
    go $ read n
    where
        go 0 = return ()
        go n = do
            s <- getLine
            let [nn,k] = map read $ words s
            print $ getRes ! (nn,k)
            go (n-1)

