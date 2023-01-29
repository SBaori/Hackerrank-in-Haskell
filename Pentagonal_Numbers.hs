-- Tail recursion!
helper :: Int -> Int -> Int -> Int
helper i n prev
  | i == (n + 1) = prev
  | otherwise = helper (i + 1) n (prev + 5 * (i - 1) - (2 * (i - 1) - 1))

solve :: Int -> Int
solve 1 = 1
solve 2 = 5
solve n = helper 3 n 5

main = do
    n <- getLine
    go $ read n
    where
        go 0 = return ()
        go n = do
            s <- getLine
            putStrLn $ show $ solve $ read s
            go (n-1) 
