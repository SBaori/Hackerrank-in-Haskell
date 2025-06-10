import Data.Bits (shiftL, shiftR, (.|.), (.&.))
import Data.Char (isSpace)

data Tree a = Node a (Tree a) (Tree a) | Empty

nextTree :: (Tree Int) -> Int -> (Tree Int)
nextTree currTree rule = helper 0 currTree
    where
        helper :: Int -> Tree Int -> Tree Int
        helper parentNode (Node val Empty Empty) = Node newVal Empty Empty
          where
            bitIndex = ((shiftL ((shiftL ((shiftL (0 .|. parentNode) 1) .|. 0) 1) .|. val) 1) .|. 0)
            newVal = (shiftR rule bitIndex) .&. 1
        helper parentNode (Node val left@(Node leftVal _ _) right@(Node rightVal _ _)) = Node newVal (helper val left) (helper val right)
          where
            bitIndex = ((shiftL ((shiftL ((shiftL (0 .|. parentNode) 1) .|. leftVal) 1) .|. val) 1) .|. rightVal)
            newVal = (shiftR rule bitIndex) .&. 1

solve :: Int -> Int -> [(Int, [Bool])] -> [Tree Int] -> Int -> [Int] -> [Int]
solve _ _ [] _ _ acc = reverse acc
solve time rule ((offset, moves):qs) prevTrees len acc
  | time + offset <= len-1 = solve (time+offset) rule qs prevTrees len ((nodeVal (prevTrees !! (len-1 -(time+offset)))):acc)
  | otherwise = solve (time+offset) rule qs prevTrees' (time+offset+1) ((nodeVal $ head prevTrees'):acc)
  where
    nodeVal :: Tree Int -> Int
    nodeVal tree = getNodeVal tree moves
    
    prevTrees' = helper prevTrees (time+offset-len+1)
    
    helper :: [Tree Int] -> Int -> [Tree Int]
    helper prevTrees 0 = prevTrees
    helper prevTrees cnt = helper ((nextTree (head prevTrees) rule):prevTrees) (cnt-1)
    
    getNodeVal :: Tree Int -> [Bool] -> Int
    getNodeVal (Node val _ _) [] = val
    getNodeVal (Node val left right) (m:ms)
      | m = getNodeVal left ms
      | otherwise = getNodeVal right ms


parse :: String -> (Int, Tree Int, [(Int, [Bool])])
parse inp = (rule, parseTree $ treeStr, parseQueries q)
  where
    linp = lines inp
    rule = read . head $ linp
    treeStr = head . tail $ linp
    q = tail . tail . tail $ linp
    
    parseQueries :: [String] -> [(Int, [Bool])]
    parseQueries = map parseQuery
      where
        parseQuery :: String -> (Int, [Bool])
        parseQuery s = 
          let [numStr, bracketStr] = words s
              num = read numStr
              inner = init (tail bracketStr)
              bools = map (=='<') inner
          in (num, bools)
    
    parseTree :: String -> Tree Int
    parseTree = fst . parseExpr . dropWhile isSpace
      where
        -- Parses either a full node or a leaf value
        parseExpr :: String -> (Tree Int, String)
        parseExpr ('(':rest) =
          let (left, rest1)  = parseExpr (dropWhile isSpace rest)
              (val, rest2)   = parseValue rest1
              (right, rest3) = parseExpr (dropWhile isSpace rest2)
          in (Node val left right, consumeClose rest3)
        parseExpr (c:rest)
          | c == '.' || c == 'X' =
              let (val, rest') = parseValue (c:rest)
              in (Node val Empty Empty, rest')
        parseExpr _ = error "Invalid expression"
        
        -- Parses a node value and returns it as Int
        parseValue :: String -> (Int, String)
        parseValue (c:rest)
          | c == '.'  = (0, dropWhile isSpace rest)
          | c == 'X'  = (1, dropWhile isSpace rest)
          | otherwise = error ("Invalid character: " ++ [c])
        parseValue [] = error "Unexpected end of input"
        
        -- Expects and consumes a ')'
        consumeClose :: String -> String
        consumeClose (')':rest) = dropWhile isSpace rest
        consumeClose _ = error "Expected ')'"

main = interact $ unlines . map (\x -> if x == 0 then "." else "X") . (\(r, t, q) -> solve 0 r q [t] 1 []) . parse
