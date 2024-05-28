import qualified Data.Map as M

-- DFS with caching (Top down dynamic programming)
-- cacheA = cache after addition branch
-- cacheAS = cache after addition followed by subtraction branch
-- cahceASM = cache after addition followed by subtraction followed by mulitplication branch
findValidExprPath :: Int -> Int -> [Int] -> Int -> M.Map (Int, Int) Bool -> M.Map (Int, Int) Bool
findValidExprPath i val list n cache
    | i == n-1 = M.fromList [((i,val), mod val 101 == 0)]
    | val < 0 || M.member (i,val) cache = cache
    | M.member (i+1, val + list !! (i+1)) cacheA && cacheA M.! (i+1, val + list !! (i+1)) = M.insert (i,val) True cacheA
    | M.member (i+1, val - list !! (i+1)) cacheAS && cacheAS M.! (i+1, val - list !! (i+1)) = M.insert (i,val) True cacheAS
    | M.member (i+1, val * list !! (i+1)) cacheASM && cacheASM M.! (i+1, val * list !! (i+1)) = M.insert (i,val) True cacheASM
    | otherwise = M.insert (i,val) False $ M.union (M.union cacheA cacheAS) cacheASM
    where
        cacheA = findValidExprPath (i+1) (val + list !! (i+1)) list n cache
        cacheAS = findValidExprPath (i+1) (val - list !! (i+1)) list n cacheA
        cacheASM = findValidExprPath (i+1) (val * list !! (i+1)) list n cacheAS

-- Takes a valid expression path, and constructs the full expression of the path
genExpr :: [Int] -> [Int] -> [String] -> [String]
genExpr [_] [a] res = show a : res
genExpr (pn1:pn2:pns) (en:ens) res
    | pn2 < pn1 = genExpr (pn2:pns) ens (["-",show en] ++ res)
    | pn2 >= pn1 && mod pn2 pn1 == 0 && div pn2 pn1 == head ens = genExpr (pn2:pns) ens (["*",show en] ++ res)
    | otherwise = genExpr (pn2:pns) ens (["+",show en] ++ res)

solve :: [String] -> String
solve [nStr, exprNumsStr] = concat $ reverse $ genExpr exprPath exprNums []
    where
        (n, exprNums) = (read nStr, map read $ words exprNumsStr)
        exprPath = map (snd.fst) $ M.toList $ findValidExprPath 0 (head exprNums) exprNums n M.empty

main :: IO ()
main = interact $ solve . lines
