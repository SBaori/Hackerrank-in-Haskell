{-# LANGUAGE Strict #-}

import qualified Data.IntMap.Strict as IM
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)

findStrongest :: Int -> IM.IntMap (IM.IntMap Int) -> Int
findStrongest i m = fst $ IM.findMax $ m IM.! i

strongestDied :: Int -> IM.IntMap (IM.IntMap Int) -> IM.IntMap (IM.IntMap Int)
strongestDied i m = IM.adjust (IM.updateMaxWithKey (\k a -> if a == 1 then Nothing else Just (a-1))) i m

recruit :: Int -> Int -> IM.IntMap (IM.IntMap Int) -> IM.IntMap (IM.IntMap Int)
recruit i c m
  | IM.member i m = IM.adjust (IM.insertWith (+) c 1) i m
  | otherwise = IM.insert i (IM.singleton c 1) m

merger :: Int -> Int -> IM.IntMap (IM.IntMap Int) -> IM.IntMap (IM.IntMap Int)
merger i j m = IM.delete j $ IM.adjust (IM.unionWith (+) (m IM.! j)) i m


solve :: [[Int]] -> IM.IntMap (IM.IntMap Int) -> [Int] -> [Int]
solve [] _ acc =  reverse acc
solve ((op:args):inps) m acc
    | op == 1 = solve inps m ((findStrongest (head args) m):acc)
    | op == 2 = solve inps (strongestDied (head args) m) acc
    | op == 3 = solve inps (recruit (head args) (last args) m) acc
    | otherwise = solve inps (merger (head args) (last args) m) acc

main :: IO ()
main = BS.interact $ BS.unlines . map (BS.pack . show) . (\inp -> solve inp IM.empty []) . parseInp
    where   
        parseInp = map (map (fst . fromJust . BS.readInt) . BS.words) . tail . BS.lines
