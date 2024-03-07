import qualified Data.Map as M
import qualified Data.Vector as V
import Data.Maybe (fromMaybe)
import Data.List (tails, foldl', concatMap)

getPassSplit :: String -> [String] -> [String]
getPassSplit loginStr pass = V.last dp
    where
        loginStrDp = V.fromList $ zip (reverse $ tails loginStr) [0..]
        charMap = foldl' (\cm p@(c:cs) -> M.insertWith (++) c [(p, length p)] cm) M.empty pass

        dp :: V.Vector [String]
        dp = V.generate (length loginStr + 1) genVal

        genVal 0 = [""]
        genVal i = helper $ loginStrDp V.! i
            where
                helper (currLoginStr@(c:cs),len) = 
                    foldr (\(p,len') res -> if (len >= len') && (p == take len' currLoginStr) && not (null (dp V.! (len-len'))) then 
                                                p:(dp V.! (len-len')) 
                                            else 
                                                res
                          ) [] $ fromMaybe [] $ M.lookup c charMap

main :: IO ()
main = do
    n <- getLine
    go $ read n
    where
        go 0 = return ()
        go n = do
            _ <- getLine
            passStr <- getLine
            loginStr <- getLine
            let format a = if null a then "WRONG PASSWORD" else tail $ concatMap (\w -> if null w then w else ' ':w) a
            putStrLn $ format $ getPassSplit loginStr $ words passStr
            go (n-1)
