num :: Int
num = 100000007

modsum :: [Int] -> Int
modsum = foldr (\x y -> mod (mod x num + mod y num) num) 0

-- Memoized list! Take a look at the memoized fibonacci problem to get a better understanding.
solve :: Int -> Int
solve = (map solve' [0..] !!) 
    where
        solve' 0 = 1
        solve' 1 = 1
        solve' x = modsum [solve (i-1) * solve (x-i)| i <- [1..x]]

main :: IO ()
main = do
    n <- getLine
    go $ read n
    where
        go 0 = return ()
        go n = do
            s <- getLine
            print $ func $ read s
            go (n-1)
