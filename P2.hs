module P2 where
import Algorithms 

--PGM
data Image = Image {
                    width :: Int, 
                    height :: Int,
                    maxValue :: Int,
                    content :: [[Int]] 
                    } deriving (Show, Read)


example = Image 24 7 15 [[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
                         [3,3,3,3,0,0,7,7,7,7,0,0,11,11,11,11,0,0,15,15,15,15,0,3],
                         [0,0,0,0,0,7,0,0,0,0,0,11,0,0,0,0,0,15,0,0,15,0,3,3],
                         [3,0,0,0,7,7,7,0,0,0,11,11,11,0,0,0,15,15,15,15,0,3,0,0],
                         [0,0,0,7,0,0,0,0,0,11,0,0,0,0,0,15,0,0,0,0,3,0,0,0],
                         [0,0,7,7,7,7,0,0,11,11,11,11,0,0,15,0,0,0,0,0,0,0,0,0],
                         [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]]

removeComments :: [String] -> [String]
removeComments = filter (\str -> head str /= '#')

separateList :: [Int] -> Int -> Int -> [[Int]]
separateList [] _ _ = []
separateList list w h = take w list: separateList (drop w list) w (h-1)

makeContent :: [String] -> Int -> Int -> [[Int]]
makeContent str w h = 
    let
        oneString = foldl (++) [] str
        wordsStr = words oneString
        listInt = map (\elem -> read elem :: Int) wordsStr
    in 
        separateList listInt w h

makeImage :: String -> IO Image
makeImage str = do
    let
        listStr = removeComments (lines str)
        wordsStr = words (listStr!!1) 
        w = read (head wordsStr)
        h = read (head (tail wordsStr))
        maxValue = read (listStr!!2) :: Int
        c = makeContent (drop 3 listStr) w h 
    return (Image w h maxValue c)

loadImage :: String -> IO Image
loadImage path = do
            str <- readFile path 
            makeImage str 

-------------------------------------------------

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
makeString img = "P2\n" ++ show (width img) ++ " " ++ show (height img) ++ "\n" ++ show (maxValue img) ++ "\n" ++ getContent (content img)

saveImage :: FilePath -> Image -> IO()
saveImage path img = writeFile path (makeString img)

------------------------------------------------------------

-- first algorithm

makePixelRow :: [Int] -> Int -> Int -> [Int]
makePixelRow [] _ _ = []
makePixelRow (x:xs) error maxValue = 
    let
        currentPixel = x
        tuple = roundPixel (currentPixel + error) maxValue
        newPixelValue = fst tuple
        newError = snd tuple
    in newPixelValue : makePixelRow xs newError maxValue

makePixelMatrix :: [[Int]] -> Int -> [[Int]]
makePixelMatrix matrix maxValue = map (\row -> makePixelRow row 0 maxValue) matrix

apply1D :: Image -> Image
apply1D img = Image (width img) (height img) (maxValue img) (makePixelMatrix (content img) (maxValue img))


-----------------------------------------------------------------------------------------

apply :: Image -> Algorithm -> Image
apply img alg
    | alg == OneDimensional = apply1D img
    | alg == FloydSteinberg || alg == JarvisJudiceNinke || alg == Stucki || alg==Atkinson || alg == SierraLite || alg == Burkes = 
        let
            w = width img
            h = height img
            max = maxValue img
            c = makeMatrix 0 0 alg (content img) max
        in Image w h max c
    | otherwise = error "Sorry, this algorithm isn't supported yet :/"

