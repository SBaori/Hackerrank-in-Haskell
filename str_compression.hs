convert :: Int -> String -> String
convert 1 [c] = [c]
convert curr [c] = [c] ++ (show curr)
convert curr (c1:c2:cs)
    | (c1 == c2) = convert (curr + 1) (c2:cs)
    | otherwise = [c1] ++ num ++ convert 1 (c2:cs)
        where
            num = if curr == 1 then [] else (show curr)

main = interact $ convert 1 . head . words
