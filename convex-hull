{-# LANGUAGE MultiWayIf #-}

-- Max value of x and y
maxp :: Floating a => a
maxp = 10001

-- Distance between 2 points
dist :: (Floating a) => ((a,a),(a,a)) -> a
dist pair = sqrt $ ((x2 - x1)**2 + (y2 - y1)**2)
    where
        x1 = fst $ fst pair
        y1 = snd $ fst pair
        x2 = fst $ snd pair
        y2 = snd $ snd pair

-- slope of 2 points
slope :: (Floating a,Ord a) => (a,a) -> (a,a) -> a
slope p1 p2
    | ((y1 == y2) && (x1 == x2)) = 0
    | (x1 == x2) = 1/0
    | otherwise = (y2-y1) / (x2-x1)
    where
        x1 = fst p1
        x2 = fst p2
        y1 = snd p1
        y2 = snd p2

-- Angle between two lines 
lineAngle :: (Floating a,Ord a) => a -> a -> a
lineAngle m1 m2
    | (m1 == m2) = 0
    | (m1 == (1/0)) || (m1 == (-1/0)) = ((m2/m1)-1)/(1/m1 + m2)
    | (m2 == (1/0)) || (m2 == (-1/0))= (1-(m1/m2))/(1/m2 + m1)
    | m1*m2 == -1 = 1/0
    | otherwise = (m2 - m1) / (1 + m1*m2)

-- Finds the bottom left most point and puts at the start of list (initializing for commencement of the algorithm)
start :: (Ord a,Floating a) => [(a,a)] -> [(a,a)]
start points = [pivot] ++ filter (/=pivot) points
    where
        pivot = foldl (\(x1,y1) (x2,y2) -> if ((y1>y2) || ((y1 == y2) && (x1>x2))) then (x2,y2) else (x1,y1)) (maxp,maxp) points

-- Given a list of points, a pivot, and previous edge slope, this find the next point in the hull
nextPoint :: (Ord a,Floating a) => [(a,a)] -> (a,a) -> a -> (a,a) -> (a,a)
nextPoint [] minPoint _ _ = minPoint
nextPoint (p:ps) minPoint edgeSlope pivot
    | (lineAng>=0) = if 
        | (minLineAng < 0) || (lineAng < minLineAng) || ((lineAng == minLineAng) && (d > minDist)) -> nextPoint ps p edgeSlope pivot
        | otherwise -> nextPoint ps minPoint edgeSlope pivot 
    | otherwise = if
        | (minLineAng < 0) && ((lineAng < minLineAng) || ((lineAng == minLineAng) && (d > minDist)))-> nextPoint ps p edgeSlope pivot
        | otherwise -> nextPoint ps minPoint edgeSlope pivot
        where
            s = slope pivot p
            minAng = slope pivot minPoint
            d = dist (pivot,p)
            minDist = dist (pivot,minPoint)
            lineAng = lineAngle edgeSlope s
            minLineAng = lineAngle edgeSlope minAng

-- This Iterates over all the points, finds and appends the hull points and returns the list (Jarvis March Algorithm) 
convexHull :: (Ord a,Floating a) => [(a,a)] -> a -> (a,a) -> [(a,a)]
convexHull points edgeSlope pivot
    | pivot == head points = [pivot]
    | otherwise = [pivot] ++ (convexHull (rest') nextSlope hullPnt)
    where
        hullPnt
            | (tail points) == [] = head points
            | otherwise = nextPoint (points) (head $ tail points) edgeSlope pivot
        nextSlope = slope pivot hullPnt
        rest' = [head points] ++ (filter (\x -> if ((slope pivot x) /= nextSlope) then True else False) (tail points))       

-- Given a list of pair of points, it calculates the sum of distances of each pair
perimtr :: (Floating a) => [((a,a),(a,a))] -> a
perimtr [] = 0
perimtr (h:hs) = (dist h) + perimtr (hs)

-- Given a list of points, it creates a cyclic pair and passes it to perimtr function and returns the perimeter of the polygon.
hullPeri :: (Floating a) => [(a,a)] -> a
hullPeri hull = perimtr $ zip hull (tail $ cycle hull)

-- Finds the first two hull points and then calls and passes on to convexHull function. (Some more initializing to prevent edge cases)
solve :: [(Int,Int)] -> Double
solve points = hullPeri ([p1] ++ convexHull (fin) s2 p2)
    where
        holyPoints = map (\(x,y) -> (fromIntegral x,fromIntegral y)) points
        initPoints = start holyPoints
        p1 = nextPoint (tail initPoints) (head $ tail initPoints) 0 (head initPoints)
        s1 = slope (head initPoints) p1
        rest = tail initPoints
        rest' = (filter (\x -> if ((slope (head initPoints) x)/=(s1)) then True else False)) (rest)
        p2 = nextPoint (rest') (head rest') (slope (head initPoints) p1) p1
        s2 = slope p1 p2
        rest'' = (filter (\x -> if ((slope p1 x)/=(s2)) then True else False)) (rest')
        fin = [head initPoints] ++ rest'' 

-- Takes a list of integers and converts it to (x,y) pairs (coordinates)
getList :: [Int] -> [(Int,Int)]
getList [] = []
getList (p1:p2:ps) = [(p1,p2)] ++ getList ps

main = interact $ show . solve . getList . map (read :: String -> Int) . words
