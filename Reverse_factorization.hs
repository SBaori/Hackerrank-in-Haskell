import Data.List (sortBy, tails)

format :: [Int] -> String
format [] = "-1"
format f = tail $ concatMap (\x -> ' ':show x) f

solve :: ([Int], Int) -> [Int]
solve (s,n) = foldl (\r partS -> 
                                let 
                                    f = factors partS n [] 
                                    lenf = length f
                                    lenr = length r
                                    check = mod n (head partS) == 0
                                in 
                                    if not check || null f || (not (null r) && lenr < lenf) then 
                                        r 
                                    else if null r || lenr > lenf then
                                        f
                                    else
                                        min r f
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
