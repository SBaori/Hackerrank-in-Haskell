dp :: [[[(Int,Int)]]]
dp = initTable : (nextTable <$> dp)
    where
        initTable = ((1,0) : repeat (1,0)) : repeat ((0,1) : repeat (0,0))

        nextTable :: [[(Int,Int)]] -> [[(Int,Int)]]
        nextTable prevTable = let t = initRow : zipWith nextRow (zip prevTable (tail prevTable)) t in t
            where
                initRow = (0,0) : repeat (1,0)

        nextRow :: ([(Int,Int)], [(Int,Int)]) -> [(Int,Int)] -> [(Int,Int)]
        nextRow (prevTablePrevRow, prevTableCurRow) curTablePrevRow = let r = initCell : zipWith3 nextCell (zip (tail prevTablePrevRow) prevTableCurRow) (tail curTablePrevRow) r in r
            where
                initCell = (0,1)

        nextCell :: ((Int,Int), (Int,Int)) -> (Int,Int) -> (Int,Int) -> (Int,Int)
        nextCell (prevTablePrevRowCell, prevTableCurrRowCell) currTablePrevRowCell currTableCurrRowCell = (b+d,a+c)
            where
                a = fst prevTablePrevRowCell
                b = snd prevTableCurrRowCell
                c = snd currTablePrevRowCell
                d = fst currTableCurrRowCell

main :: IO ()
main = do
  n <- getLine
  go $ read n
  where
    go 0 = return ()
    go i = do
        s <- getLine
        let [n,m,k] = map read $ words s
        let (u,d) = dp !! k !! (n-1) !! (m-1)
        print $ mod (u+d) 1000000007
        go (i-1)
