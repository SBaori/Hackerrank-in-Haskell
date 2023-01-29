-- Tail recursion!
f :: Int -> Int -> Int -> Int
f i n prev
  | i == (n + 1) = prev
  | otherwise = f (i + 1) n (prev + 5 * (i - 1) - (2 * (i - 1) - 1))

helper :: Int -> Int
helper 1 = 1
helper 2 = 5
helper n = f 3 n 5

main = do
    n <- getLine
    go $ read n
    where
        go 0 = return ()
        go n = do
            s <- getLine
            putStrLn $ show $ (helper) $ read s
            go (n-1) 
