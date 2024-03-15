import Data.List (sortBy, tails)

format :: [Int] -> String
format [] = "-1"
format f = tail $ concatMap (\x -> ' ':show x) f

minList :: [Int] -> [Int] -> [Int]
minList [] l2 = l2
minList l1 [] = l1
minList l1 l2
    | length l1 > length l2 = l2
    | length l2 > length l1 = l1
    | otherwise = min l1 l2

solve :: ([Int], Int) -> [Int]
solve (s,n) = foldl (\r partS -> 
                        if mod n (head partS) /= 0 then 
                            r 
                        else 
                            minList r $ factors partS n [] 
                    ) [] $ init $ tails rs
    where
        rs = sortBy (flip compare) s

        factors :: [Int] -> Int -> [Int] -> [Int]
        factors _ 1 acc = 1:acc
        factors [] _ _ = []
        factors s@(a:as) n acc
            | mod n a == 0 = factors s (div n a) (n:acc)
            | otherwise = factors as n acc

parse :: [String] -> ([Int],Int)
parse inp = (a,n)
    where
        a = map (read :: String -> Int) $ drop 2 inp
        n = read $ head inp

main :: IO ()
main = interact $ format . solve . parse . words
