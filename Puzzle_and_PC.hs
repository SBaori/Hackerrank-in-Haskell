import Data.List (maximumBy, sort)

-- Quandrants are defined as follows: 
-- FST | SND
-- TRD | FTH
data Quandrant = FSTQ | SNDQ | TRDQ | FTHQ
  deriving (Eq)

-- Note: (+x): down, (+y): right

-- reflect a tromino along y = a
reflecty a = (map . map) (\(x,y) -> (x, 2*a - y - 1))

-- reflect a tromino along x = a
reflectx a = (map . map) (\(x,y) -> (2*a - x - 1, y))

-- reflect a tromino along x = a and y = b
reflectxy a b = (reflectx a) . (reflecty b)

-- Infinite list of recursively increasing trominos starting at (0,0)
tilings :: [[[(Int,Int)]]]
tilings = [[(0,0), (0,1), (1,0)]] : (newTilings <$> tilings)
  where
    newTilings tile = tile ++ (translate l tile) ++ (reflectx a tile) ++ (reflecty a tile)
      where
        a = (+1) $ snd $ maximumBy (\(x1,y1) (x2,y2) -> compare y1 y2) $ concat tile
        l = div (a+1) 2
    
    translate l = (map . map) (\(x,y) -> (x+l,y+l))

getEmptyCellQuad :: (Int,Int) -> Int -> (Int,Int) -> Quandrant
getEmptyCellQuad firstCoord@(x,y) len emptyCoord@(x',y')
  | (x' - xo) >= 0 && (y' - yo) >= 0 = FTHQ
  | (x' - xo) >= 0 && (y' - yo) < 0 = TRDQ
  | (x' - xo) <= 0 && (y' - yo) >= 0 = SNDQ
  | otherwise = FSTQ
  where
    (xo, yo) = (x + (div len 2), y + (div len 2))

-- This where we start placing the trominos recursively using the "tilings" function starting at firstCoord
solve :: Int -> (Int, Int) -> (Int,Int) -> [[(Int,Int)]]
solve 1 firstCoord emptyCellPos = []
solve i firstCoord@(fx,fy) emptyCellPos
  | quadrant == FSTQ = (reflectxy (fx + i2) (fy + i2) tiles) 
                            ++ (solve i2 firstCoord emptyCellPos)
                            
  | quadrant == SNDQ = (reflectx (fx + i2) tiles)
                            ++ (solve i2 (fx, fy + i2) emptyCellPos)
                            
  | quadrant == TRDQ = (reflecty (fy + i2) tiles) 
                            ++ (solve i2 (fx  + i2, fy) emptyCellPos)

  | otherwise = tiles ++ (solve i2 (fx + i2, fy + i2) emptyCellPos)
  where
    i2 = div i 2
    index = (\x -> x-1) $ truncate $ logBase 2 $ fromIntegral i
    tiles = (map . map) (\(x,y) -> (x+fx, y+fy)) $ tilings !! index
    quadrant = getEmptyCellQuad (fx, fy) i emptyCellPos

-- format the list of tromino coordinates according to the required output.
showTiles :: [[(Int,Int)]] -> String
showTiles = unlines . map (unwords . map showCoord . sort)
  where
    showCoord :: (Int,Int) -> String
    showCoord (x,y) = show x ++ " " ++ show y

main :: IO ()
main = interact $ \input ->
    let (nLine:xLine:_) = lines input
        n = read nLine
        [x, y] = map read (words xLine)
    in showTiles $ (solve (fromIntegral (2^n)) (1,1) (x,y))
