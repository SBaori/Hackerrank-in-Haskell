import qualified Data.Map.Strict as M
import qualified Data.Vector as V

solve' :: V.Vector Int -> [Int]
solve' nums = map (\(_ ,_ ,_ , median) -> median) $ V.toList trail
  where
    trail = V.generate (V.length nums) currState
    
    currState :: Int -> (M.Map Int Int, M.Map Int Int, Int, Int)
    currState ind
      | ind == 0 = (M.singleton num 1, M.empty, 1, num)
      | num >= 0 = (l', r', count+1, (fst $ M.findMax l'))
      | otherwise = trail V.! (ind + num)
      where
        (l, r, count, median) = trail V.! (ind - 1)
        num = nums V.! ind
        
        (l', r')
          | even count = if num <= (fst $ M.findMin r) then (M.insertWith (+) num 1 l, r) else transactRtoL num
          | otherwise = if num > (fst $ M.findMax l) then (l, M.insertWith (+) num 1 r)  else transactLtoR num
        
        transactRtoL :: Int -> (M.Map Int Int, M.Map Int Int)
        transactRtoL num = (M.insertWith (+) (fst $ M.findMin r) 1 l, 
                            M.insertWith (+) num 1 $ M.updateMin (\v -> if v == 1 then Nothing else Just (v-1)) r)
        
        transactLtoR :: Int -> (M.Map Int Int, M.Map Int Int)
        transactLtoR num = (M.insertWith (+) num 1 $ M.updateMax (\v -> if v == 1 then Nothing else Just (v-1)) l,
                            M.insertWith (+) (fst $ M.findMax l) 1 r)

main :: IO ()
main = interact $ unlines . map show . solve' . V.fromList . map read . tail . lines
