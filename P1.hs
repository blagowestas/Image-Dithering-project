module P1 where

import Algorithms ( Algorithm )


--PBM
data Image = Image {
                    width :: Int,
                    height :: Int,
                    content :: [[Int]] -- 0 or 1
                    } deriving (Read, Show)

removeComments :: [String] -> [String]
removeComments = filter (\str -> head str /= '#')

getPixels :: [String] -> [[Int]]
getPixels = map (\str -> map (\ch -> if ch == '0' then 0 else 1) (filter (\ch -> ch /=' ') str))

makeImage :: String -> IO Image
makeImage str = do
    let
        listStr = removeComments (lines str)
        wordsStr = words (listStr!!1) 
        w = read (head wordsStr)
        h = read (head (tail wordsStr))
        c = getPixels (drop 2 listStr)
    return (Image w h c)

loadImage :: String -> IO Image
loadImage path = do
            str <- readFile path 
            makeImage str 

---------------------------------------------------


getRow :: [Int] -> Int -> String 
getRow []     _ = ""
getRow (x:xs) count =
    let
        num = length (show x) + 2 
        numOfAllChars = count + num
    in
        if numOfAllChars <= 76 then show x ++ "  " ++ getRow xs numOfAllChars else "\n" ++ show x ++ "  " ++ getRow xs num


getContent :: [[Int]] -> String
getContent []     = ""
getContent (x:xs) = getRow x 0 ++ "\n" ++ getContent xs

makeString :: Image -> String
makeString img = "P1\n" ++ show (width img) ++ " " ++ show (height img) ++ "\n" ++ getContent (content img)

saveImage :: FilePath -> Image -> IO()
saveImage path img = writeFile path (makeString img)

------------------------------------------------------------

applyAlgorithm :: String -> Algorithm -> String -> IO ()
applyAlgorithm path _ newPath = do
                            img <- loadImage path
                            saveImage newPath img 

                      
apply :: Image -> Algorithm -> Image
apply img _ = img