-- Memoized Fibonacci List
fib :: [Int]
fib = 0:1:zipWith (\x y -> mod (x+y) (10^8+7)) (tail fib) fib

helper :: Int -> Int
helper x = last $ take (x+1) fib

main = do
    n <- getLine
    go $ read n
    where
        go 0 = return ()
        go n = do
            s <- getLine
            putStrLn $ show $ helper $ read s
            go (n-1)
