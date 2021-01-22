--module Main where
import Data.Word ( Word8 )

data Rgb = Rgb { red   :: Word8, 
                 green :: Word8, 
                 blue  :: Word8 } deriving (Show,Read)

instance Eq Rgb where 
                    Rgb red1 green1 blue1 == Rgb red2 green2 blue2 = 
                        red1 == red2 && 
                        green1 == green2 && 
                        blue1 == blue2

data Image = Image { width   :: Int,
                     height  :: Int,
                     content :: [[Rgb]] } deriving (Show,Read)

instance Eq Image where
                     Image width1 height1 content1 == Image width2 height2 content2 = 
                         width1 == width2 && 
                         height1 == height2 && 
                         content1 == content2


data Algorithm = OneDimesional | FloydSteinberg | JarvisJudiceNinke | 
                 Stucki | Atkinson | Burkes | Sierra | TwoRowSierra |
                 SierraLite | Ordered4x4 | Ordered8x8 deriving (Show,Read,Eq)

grayExample :: Image
grayExample = Image 3 2 [[Rgb 76 76 76, Rgb 122 122 122, Rgb 227 227 227],
                         [Rgb 150 150 150, Rgb 255 255 255, Rgb 203 203 203]]

example2 :: Image
example2 = Image 4 5 [[Rgb 40 40 40, Rgb 41 41 41, Rgb 16 16 16, Rgb 40 40 40]
                , [Rgb 26 26 26, Rgb 40 40 40, Rgb 138 138 138, Rgb 73 73 73]
                , [Rgb 184 184 184, Rgb 40 40 40, Rgb 40 40 40, Rgb 40 40 40]
                , [Rgb 40 40 40, Rgb 40 40 40, Rgb 182 182 182, Rgb 40 40 40]
                , [Rgb 39 39 39, Rgb 81 81 81, Rgb 158 158 158, Rgb 15 15 15]]

ex2 = Image 4 5 [[Rgb 0 0 0, Rgb 0 0 0, Rgb 0 0 0, Rgb 255 255 255],
                 [Rgb 0 0 0, Rgb 0 0 0, Rgb 255 255 255, Rgb 0 0 0],
                 [Rgb 255 255 255, Rgb 0 0 0,Rgb 0 0 0, Rgb 0 0 0],
                 [Rgb 0 0 0, Rgb 0 0 0, Rgb 255 255 255, Rgb 0 0 0],
                 [Rgb 0 0 0, Rgb 0 0 0, Rgb 255 255 255, Rgb 0 0 0]]
-----------------------------------------------------------------------------------
getRow :: [Rgb] -> String 
getRow []     = ""
getRow (x:xs) = show (red x) ++ " " ++ show (green x) ++ " " ++ show (blue x) ++ "\n" ++ (getRow xs)

getContent :: [[Rgb]] -> String
getContent []     = ""
getContent (x:xs) = getRow x ++ getContent xs

makeString :: Image -> String
makeString img = "P3\n" ++ show (width img) ++ " " ++ show (height img) ++ "\n255\n" ++ (getContent (content img))

saveImage :: FilePath -> Image -> IO()
saveImage path img = writeFile path (makeString img)

-----------------------------------------------------------------------------------

makePixel :: String -> Rgb
makePixel str = 
    let 
        list = words str
        red   =  read (head list)
        green =  read (list!!1) 
        blue  =  read (list!!2) 
    in 
        Rgb red green blue

getPixels :: [String] -> [Rgb]
getPixels = map makePixel

--можем да проверим дали имаме грешка ако w*x/= length list
separatePixels :: Int -> Int -> [Rgb] -> [[Rgb]]
separatePixels _ _ []     = []
--separatePixels _ 0 pixels = [] 
-- separatePixels _ x []     = undefined 
separatePixels w h pixels = take w pixels: separatePixels w (h-1) (drop w pixels) 


makeImage :: String -> IO Image
makeImage str = do
    let
        listStr = lines str
        wordsStr = words (listStr!!1) 
        w = read (head wordsStr)
        h = read (head (tail wordsStr))
        c = separatePixels w h (getPixels (drop 3 listStr))
    return (Image w h c)

loadImage :: String -> IO Image
loadImage path = do
            str <- readFile path 
            makeImage str 

---------------------------------------------------------------------------------

roundPixel :: (Ord b, Num a, Num b) => b -> (a, b)
roundPixel num = if x > num then (0, num) else (255, (-x)) 
    where 
        x = 255 - num
        
-- makePixelRow :: [Rgb] -> Int -> [Rgb]
-- makePixelRow [] _ = []
-- makePixelRow (x:xs) error = 
--     let
--         currentPixel = red x
--         tuple = roundPixel (fromIntegral currentPixel + error)
--         newPixelValue = toEnum (fst tuple)
--         newError = snd tuple
--     in Rgb newPixelValue newPixelValue newPixelValue : makePixelRow xs newError

-- makePixelMatrix :: [[Rgb]] -> [[Rgb]]
-- makePixelMatrix = map (`makePixelRow` 0)  


widthMatrix :: [[a]] -> Int
widthMatrix matrix = length (head matrix)
 
getFirst :: (a, b, c) -> a
getFirst (x, _, _) = x

getSecond :: (a, b, c) -> b
getSecond (_,x,_) = x

getThird :: (a, b, c) -> c
getThird (_,_,x) = x


--queue is a list from tuples with row, col and error to visit
-- makeMatrixFS :: [[Rgb]] -> [(Int,Int,Int)] -> [[Rgb]]
-- makeMatrixFS matrix [] = matrix 
-- makeMatrixFS matrix queue = 
--     let 
--         currentTuple = head queue
--         row = getFirst currentTuple
--         col = getSecond currentTuple
--         error = getThird currentTuple -- новия ерър не е толкова!
--         currentPixel = red (matrix!!row!!col)
--         newPixelValue = fst (roundPixel (fromIntegral currentPixel + error)) --може да го оправя да не връща двойка, когато отделям алгоритмите
--         newPixel = Rgb newPixelValue newPixelValue newPixelValue
        
--         newQueue 
--              | row /= length matrix && col /= widthMatrix matrix = tail queue ++ [(row, col + 1, error* div 7 16), (row + 1, col - 1, error * div 3 16), (row+1, col, error * div 5 16), (row+1, col+1, div error 16)]
--              | row /= length matrix && col == widthMatrix matrix = tail queue ++ [(row + 1, col - 1, error * div 3 16), (row+1, col, error * div 5 16)]
--              | row == length matrix && col /= widthMatrix matrix = tail queue ++ [(row, col + 1, error* div 7 16)]
--              | otherwise = []
        
--         newMatrix = take row matrix ++  [(take col matrix!!row) ++ [newPixel] ++ (drop (col+1) matrix!!row)] ++ drop (row + 1) matrix
--     in makeMatrixFS newMatrix newQueue


-- makeImgFS :: Image -> Image
-- makeImgFS img = Image (width img) (height img)(makeMatrixFS (content img) [(0,0,0)])  

-- floydSteinberg :: String -> Image
-- floydSteinberg path = let img = loadImage path
--                        makeImgFS img


makeQueue :: Algorithm -> Int -> Int-> Int -> [(Int, Int, Int)]
makeQueue alg error row col 
    | alg == FloydSteinberg    = [(row, col + 1, error* div 7 16), (row + 1, col - 1, error * div 3 16), (row+1, col, error * div 5 16), (row+1, col+1, div error 16)]

    | alg == JarvisJudiceNinke = [(row, col+1,error * div 7 48),   (row, col+2, error * div 5 48),       (row+1, col-2, error * div 3 48),(row+1, col-1, error * div 5 48), 
                                  (row+1, col, error * div 7 48),  (row+1, col+1, error * div 5 48),     (row+1, col+2, error * div 3 48),(row+2, col-2, error * div 1 48),
                                  (row+2, col-1, error * div 3 48),(row+2, col, error * div 5 48),       (row+2, col+1,error * div 3 48), (row+2, col+2, error * div 1 48)]
  
    | alg == Stucki            = [(row, col+1,error * div 8 42),   (row, col+2, error * div 4 42),       (row+1, col-2, error * div 2 42),(row+1, col-1, error * div 4 42), 
                                  (row+1, col, error * div 8 42),  (row+1, col+1, error * div 4 42),     (row+1, col+2, error * div 2 42),(row+2, col-2, error * div 1 42),
                                  (row+2, col-1, error * div 2 42),(row+2, col, error * div 4 42),       (row+2, col+1,error * div 2 42), (row+2, col+2, error * div 1 42)]
      
    | alg == Atkinson          = [(row, col+1, div error 8),       (row, col+2, div error 8),            (row+1, col-1, div error 8),     (row+1, col, div error 8), 
                                  (row+1, col+1, div error 8),     (row+2, col, div error 8)]
  
    | alg == Burkes            = [(row, col+1, error * div 8 32),  (row, col+2, error * div 4 32),       (row+1, col-2, error * div 2 32), (row+1, col-1, error * div 4 32), 
                                  (row+1, col, error * div 8 32),  (row+1, col+1, error * div 4 32),     (row+1, col+2, error * div 2 32)]

    | alg == Sierra            = [(row, col+1, error * div 5 32),  (row, col+2, error * div 3 32),       (row+1, col-2, error * div 2 32), (row+1, col-1, error * div 4 32),
                                  (row+1, col, error * div 5 32),  (row+1, col+1, error * div 4 32),     (row+1, col+2, error * div 2 32), (row+2, col-1, error * div 2 32),
                                  (row+2, col, error * div 3 32),  (row+2, col+1, error * div 2 32)]   

    | alg == TwoRowSierra      = [(row, col+1, error * div 4 16),  (row, col+2, error * div 3 16),       (row+1, col-2, error * div 1 16), (row+1, col-1, error * div 2 16),   
                                  (row+1, col, error * div 3 16),  (row+1, col+1, error * div 2 16),     (row, col+2, error * div 1 16)]

    | alg == SierraLite        = [(row, col+1, error * div 2 4),   (row+1, col-1, error * div 1 4),      (row+1, col, error * div 1 4)]   

    | alg == Ordered4x4 = []

    | alg == Ordered8x8 = []

    | otherwise = undefined -- error: greshen algorithm

-- makeMatrix :: [[Rgb]] -> [(Int,Int,Int)] -> Algorithm -> [[Rgb]]
-- makeMatrix matrix [] _ = matrix 
-- makeMatrix matrix queue alg = 
--     let
--         currentTuple = head queue
--         row = getFirst currentTuple
--         col = getSecond currentTuple
--         error = getThird currentTuple
--     in
--         if row >= 0 && row < length matrix && col >= 0 && col < widthMatrix matrix 
--             then 
--                 let 
--                     currentPixel = red (matrix!!row!!col)
--                     tuple = roundPixel (fromIntegral currentPixel + error)
--                     newPixelValue = fst tuple
--                     newErrorValue = snd tuple
--                     newPixel = Rgb newPixelValue newPixelValue newPixelValue
--                     newQueue = tail queue ++ makeQueue alg newErrorValue row col
--                     newMatrix = take row matrix ++ [(take col matrix!!row) ++ [newPixel] ++ (drop (col+1) matrix!!row)] ++ drop (row + 1) matrix
--                 in makeMatrix newMatrix newQueue alg
--             else makeMatrix matrix (tail queue) alg
         

make :: [[Rgb]] -> [[Rgb]]
make = map (map (\pixel -> let 
                             newPixelValue = fst (roundPixel (fromIntegral (red pixel)))
                           in   
                             Rgb newPixelValue newPixelValue newPixelValue )) 


--purwo prawq nowa matrica s subranite pixeli ??? te shte izlizat izwun 255 trqbwa da se oprawiiiii
sumPixels :: [[Rgb]] -> [(Int,Int,Int)] -> Algorithm -> [[Rgb]]
sumPixels matrix [] _ = make matrix
sumPixels matrix queue alg = 
    let 
        row = getFirst (head queue)
        col = getSecond (head queue)
    in 
        if row >= 0 && row < length matrix && col >= 0 && col < widthMatrix matrix 
            then 
                let 
                    error = getThird (head queue)
                    currentPixel = red (matrix!!row!!col)
                    newPixelValue = fromIntegral currentPixel + error
                    newError = snd (roundPixel (fromIntegral newPixelValue))
                    newPixel = if newPixelValue < 0 then Rgb 0 0 0 else if newPixelValue > 255 then Rgb 255 255 255 else Rgb (toEnum newPixelValue) (toEnum newPixelValue) (toEnum newPixelValue)
                    newQueue = tail queue ++ makeQueue alg newError row col
                    newMatrix = take row matrix ++ [(take col (matrix!!row)) ++ [newPixel] ++ (drop (col+1) (matrix!!row))] ++ drop (row + 1) matrix
                in sumPixels newMatrix newQueue alg
            else sumPixels matrix (tail queue) alg


makeImg :: Image -> Algorithm -> Image
makeImg img alg = Image (width img) (height img) (sumPixels (content img) [(0,0,0)] alg) 

isValid :: String -> Algorithm -> String -> Bool
isValid path alg output = True 


-- applyAlgorithm :: String -> Algorithm -> Image
-- --applyAlgorithm path OneDimesional     = oneDimesional path
-- --applyAlgorithm path FloydSteinberg    = floydSteinberg path
-- applyAlgorithm path JarvisJudiceNinke = jarvisJudiceNinke path
-- applyAlgorithm path Stucki            = stucki path
-- applyAlgorithm path Atkinson          = atkinson path
-- applyAlgorithm path Burkes            = burkes path
-- applyAlgorithm path Sierra            = sierra path
-- applyAlgorithm path TwoRowSierra      = twoRowSierra path
-- applyAlgorithm path SierraLite        = sierraLite path
-- applyAlgorithm path Ordered4x4        = ordered4x4 path
-- applyAlgorithm path Ordered8x8        = ordered8x8 path


-- imageDithering :: String -> Algorithm -> String -> IO ()
-- imageDithering path algorithm output = 
--     if isValid path algorithm output 
--         then saveImage output (makeImg (makeImage path) algorithm)  
--         else undefined  

