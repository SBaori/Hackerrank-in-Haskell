import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (group, foldl', transpose, permutations)
import Control.Monad (guard)

getPlaces :: [String] -> [[(Int,Int)]]
getPlaces inp = allPlaces
  where
    horizontalPlaces = foldl' (\s r -> addPlaces r s True) [] $ zip [0..] inp
    allPlaces = foldl' (\s r -> addPlaces r s False) horizontalPlaces $ zip [0..] $ transpose inp
	
    addPlaces :: (Int, String) -> [[(Int,Int)]] -> Bool -> [[(Int,Int)]]
    addPlaces (rNum, row) places isHorizontal = places'
      where
        grouped = group row
        
        places' = helper 0 places grouped
        
        helper :: Int -> [[(Int,Int)]] -> [String] -> [[(Int,Int)]]
        helper _ places [] = places
        helper cNum places (c:cs)
          | head c == '-' && length c > 1 = helper (nextCNum) (place:places) cs
          | otherwise = helper (cNum + length c) places cs
          where
            len = length c
            nextCNum = cNum + len
            place = [if isHorizontal then (rNum, cNum') else (cNum', rNum)| cNum' <- [cNum .. nextCNum-1]]

getPlacements :: [[(Int,Int)]] -> [String] -> M.Map (Int,Int) Char
getPlacements places placeHolders = M.fromList placement
  where
    placeHoldersPerms = permutations placeHolders
    placements = zip placeHoldersPerms $ repeat places
    
    placement = do
      let validPlacements = filter (\(phs, pls) -> isValid phs pls M.empty) placements
      
      guard (not . null $ validPlacements)
      
      let (placeHolder', places') = head validPlacements
      
      (place, placeHolder) <- zip places' placeHolder'
      (ple, phe) <- zip place placeHolder
      pure (ple, phe)
    
    isValid :: [String] -> [[(Int,Int)]] -> M.Map (Int,Int) Char -> Bool
    isValid [] [] _ = True
    isValid (ph:phs) (pl:pls) occupied
      | not $ canPlace = False
      | otherwise = isValid phs pls (M.union occupied $ M.fromList $ zip pl ph)
      where
        canPlace
          | length ph /= length pl = False
          | otherwise = and $ map (\(phe,ple) -> (M.findWithDefault phe ple occupied) == phe) $ zip ph pl

constructBoard :: M.Map (Int,Int) Char -> (Int,Int) -> [String]
constructBoard placementMap (r,c) = [[M.findWithDefault '+' (x,y) placementMap | y <- [0..c-1]] | x <- [0..r-1]]

parseInp :: [String] -> ([String], [String])
parseInp inp = (board, placeHolders)
  where
    board = init inp
    placeHolders = words . map (\c -> if c == ';' then ' ' else c) . head . reverse $ inp

solve :: [String] -> String
solve inp = unlines finalBoard
  where
    (board, placeHolders) = parseInp inp
    (r,c) = (length board, length . head $ board)
    places = getPlaces board
    placements = getPlacements places placeHolders
    finalBoard = constructBoard placements (r,c)
    
main :: IO ()
main = interact $ solve . lines
