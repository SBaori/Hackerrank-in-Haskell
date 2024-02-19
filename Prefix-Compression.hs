lcp :: [Char] -> [Char] -> [String]
lcp s1 s2 = helper s1 s2 ""
    where
        helper "" s2  acc = [reverse acc,"",s2]
        helper s2 "" acc = [reverse acc,s2,""]
        helper (a:as) (b:bs) acc
            | a==b = helper as bs (a:acc)
            | otherwise = [reverse acc,a:as,b:bs]

solve :: [String] -> String
solve [s1,s2] = format $ lcp s1 s2
    where
        format = concatMap (\x -> show (length x) ++ " " ++ x ++ "\n")

main :: IO ()
main = interact $ solve . lines
