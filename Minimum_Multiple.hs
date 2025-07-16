import qualified Data.Array as A

data Tree a b = 
  Node (a,a,b) (Tree a b) (Tree a b) 
  | Empty
  deriving Show

buildTree :: Int -> Int -> (Integer -> Integer -> Integer) -> A.Array Int Integer -> Tree Int Integer
buildTree l r f arr
  | l == r = Node (l,r,arr A.! l) Empty Empty
  | otherwise = Node (l,r, f vl vr) leftNode rightNode
  where
    leftNode@(Node (ll,rl,vl) _ _ ) = buildTree l (div (l+r) 2) f arr
    rightNode@(Node (lr, rr, vr) _ _) = buildTree ((div (l+r) 2) + 1) r f arr

query :: (Int, Int) -> Tree Int Integer -> (Integer -> Integer -> Integer) -> Integer -> Integer
query (l,r) (Node (l',r',v) left right) f identity
  | l' > r || r' < l = identity
  | l <= l' && r >= r' = v
  | otherwise = f (query (l,r) left f identity) (query (l,r) right f identity)

update :: Int -> Int -> (Integer -> Integer -> Integer) -> Tree Int Integer -> Tree Int Integer
update idx val f (Node (l,r,v) left right)
  | l == r && idx == r = Node (l,r,v * (fromIntegral val)) left right
  | idx < l || idx > r = Node (l,r,v) left right
  | otherwise = Node (l,r,f vl vr) leftNode rightNode
  where
    leftNode@(Node (ll,rl,vl) _ _) = update idx val f left
    rightNode@(Node (lr,rr,vr) _ _) = update idx val f right

solve :: ([Integer], [(String, Int, Int)]) -> [Integer]
solve (arr, queries) = helper queries tree []
  where
    tree = buildTree 0 (length arr - 1) lcm . A.listArray (0, length arr - 1) $ arr
    
    helper :: [(String, Int, Int)] -> Tree Int Integer -> [Integer] -> [Integer]
    helper [] _ acc = reverse acc
    helper (q@(t,n1,n2):qs) tree acc
      | t == "Q" = helper qs tree ((mod (query (n1,n2) tree lcm 1) 1000000007):acc)
      | otherwise = helper qs (update n1 n2 lcm tree) acc

parse :: String -> ([Integer], [(String,Int,Int)])
parse inp = (arr, queries)
  where
    linp = tail . lines $ inp
    queries = map ((\[t,n1,n2] -> (t, read n1, read n2)) . words) . tail . tail $ linp
    arr = map read . words . head $ linp

formatOutput :: [Integer] -> String
formatOutput = unlines . map show

main :: IO ()
main = interact $ formatOutput . solve . parse
