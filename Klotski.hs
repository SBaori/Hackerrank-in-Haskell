import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad (guard)
import Data.List (minimumBy)

type Shape = [(Int,Int)]
type Grid = S.Set (Bool, Shape)

translate :: Shape -> (Int,Int) -> Shape
translate shape (xo,yo) = map (\(x,y) -> (x+xo,y+yo)) shape

getShapeMap :: [[String]] -> M.Map String Shape
getShapeMap grid = M.fromListWith (++) $ concat $ [[(c, [(rn,cn)]) | (cn,c) <- zip [0..] r, isShape c] | (rn,r) <- zip [0..] grid]
    where
        isShape = notElem '.'

getTopLeft :: Shape -> (Int,Int)
getTopLeft shape = (x,y)
    where
        x = fst $ minimumBy (\(x1, _) (x2, _) -> compare x1 x2) shape
        y = snd $ minimumBy (\(_, y1) (_, y2) -> compare y1 y2) shape


-- @info: MoveShapeAround
getShapeMoves :: [(Shape, Grid)] 
                  -> (Int,Int) 
                  -> Bool 
                  -> Grid 
                  -> S.Set (Grid) 
                  -> [(Shape, Grid)] 
                  -> [(Shape, Grid)]
getShapeMoves [] _ _ _ _ acc = acc
getShapeMoves ((shape, occ):qs) (r,c) isTargetLabel remGrid seenGrids acc = getShapeMoves (qs ++ newMoves) (r,c) isTargetLabel remGrid seenGrids' (acc ++ newMoves)
    where
        occupied = S.fromList $ concat $ map snd $ S.toList remGrid

        (newMoves, seenGrids') = helper [(1,0), (0,1), (-1,0), (0,-1)] seenGrids []

        -- @info: MoveShapeAroundOnce
        helper [] seenGrids acc = (acc, seenGrids)
        helper (o:os) seenGrids acc
            | isValid && isNotSeen = helper os seenGrids' ((shape', grid):acc)
            | otherwise = helper os seenGrids acc
            where
                shape' = translate shape o
                grid = S.union remGrid $ S.singleton (isTargetLabel, shape')
                isNotSeen = S.notMember grid seenGrids
                isValid = all (\(x,y) -> x >= 0 && x < r && y >= 0 && y < c && S.notMember (x,y) occupied) shape'
                seenGrids' = S.union seenGrids $ S.singleton grid

-- @info: bfs till targetShape reaches target (assuming a solution always exists)
getTargetShapePath :: [(M.Map String Shape, Grid, [(String, (Int,Int), (Int,Int))])]
                      -> (Int,Int)
                      -> (String, (Int,Int))
                      -> [String]
                      -> S.Set (Grid)
                      -> [(String, (Int,Int), (Int,Int))]
getTargetShapePath q@((shapeMap, grid, moves):qs) (r,c) target@(tLabel, (tx,ty)) labels seenGrids
    | not . null $ targetLocs = reverse $ (\(_, _, moves) -> moves) $ head targetLocs
    | otherwise = getTargetShapePath (qs ++ q') (r,c) target labels seenGrids'
    where
        -- @info: getAllNextMoves of Shape
        helper [] seenGrids acc = (seenGrids, acc)
        helper (l:ls) seenGrids acc = helper ls seenGrids' (acc ++ nextShapeMoves)
            where
                shape = shapeMap M.! l
                remGrid = S.difference grid $ S.singleton (l== tLabel, shape)

                nextShapeMoves = do
                    (shape', grid') <- (getShapeMoves [(shape, grid)] (r,c) (l == tLabel) remGrid S.empty [])

                    guard (S.notMember grid' seenGrids)

                    let shapeMap' = M.insert l shape' shapeMap
                    let prevTopLeft = getTopLeft shape
                    let prevTopLeft' = getTopLeft shape'

                    pure (shapeMap', grid', (l, prevTopLeft, prevTopLeft'):moves)

                seenGrids' = S.union seenGrids $ S.fromList $ map (\(_,grid,_) -> grid) nextShapeMoves

        (seenGrids', q') = helper labels seenGrids []
        
        targetLocs = filter ((\sm -> getTopLeft (sm M.! tLabel) == (tx, ty)) . (\(f,_,_) -> f)) q'



solve :: [String] -> String
solve inp = getOutput moves
  where
    ((r,c), target@(tLabel, (tx,ty)), board) = parseInp inp
    shapeMap = getShapeMap board
    grid = S.fromList $ map (\(l, loc) -> (l == tLabel, loc)) $ M.toList shapeMap
    initCheck = (getTopLeft (shapeMap M.! tLabel)) == (tx, ty)
    
    moves = if initCheck then [] else getTargetShapePath [(shapeMap, grid, [])] (r,c) target (M.keys shapeMap) S.empty

    getOutput :: [(String, (Int,Int), (Int,Int))] -> String
    getOutput moves = show (length moves) ++ "\n" ++ (unlines movesString)
      where
        movesString = map (\(label, start, end) -> label ++ " " ++ show start ++ " " ++ show end) moves
    
    parseInp :: [String] -> ((Int,Int), (String, (Int,Int)), [[String]])
    parseInp inp = ((r,c), target, board)
      where
          ([r,c], rest) = (\([f],s) -> (map read $ words f, s)) $ splitAt 1 inp
          (board, rest') = (\(f,s) -> (map words f, s)) $ splitAt r rest
          target = (\[f,s] -> (f, (\[x,y] -> (x,y)) $ map read $ words s)) rest'


main :: IO ()
main = interact $ solve . lines
