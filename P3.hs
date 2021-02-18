module P3 where
import Data.Word

import Algorithms 

--PPM
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


----------------------------------------------------------------------------------
example :: Image
example = Image 3 2 [[Rgb 255 0 0, Rgb 155 128 0,   Rgb 255 255 0],
           [Rgb 0 255 0, Rgb 255 255 255, Rgb 128 255 128]]


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


toInt :: (RealFrac a, Integral b) => a -> b
toInt num = round num 

enumGrayPixel :: Rgb -> Rgb
enumGrayPixel pixel = Rgb { red = x, green = x, blue = x }
                      where x = toEnum (toInt ((0.30 * fromInteger (toInteger (red pixel))) +  (0.59 * fromInteger (toInteger (green pixel))) + (0.11 * fromInteger (toInteger (blue pixel)))))

 
grayscale :: Image -> Image
grayscale img = Image { width = width img,
                        height = height img,
                        content = newContent }
                where
                newContent = map (map enumGrayPixel) 
                            (content img)

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

separatePixels :: Int -> Int -> [Rgb] -> [[Rgb]]
separatePixels _ _ []     = []
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

convertImage :: IO Image -> IO ()
convertImage ioImage = do 
                img <- ioImage 
                print img

---------------------------------------------------------------------------------
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
-- first algorithm

makePixelRow :: [Rgb] -> Int -> [Rgb]
makePixelRow [] _ = []
makePixelRow (x:xs) error = 
    let
        currentPixel = red x
        pair = roundPixel (fromIntegral currentPixel + error) 255
        newPixelValue = toEnum (fst pair)
        newError = snd pair
    in Rgb newPixelValue newPixelValue newPixelValue : makePixelRow xs newError

makePixelMatrix :: [[Rgb]] -> [[Rgb]]
makePixelMatrix = map (`makePixelRow` 0)  

apply1D :: Image -> Image
apply1D img = Image (width img) (height img) (makePixelMatrix (content img))


-----------------------------------------------------------------------------------------

apply :: Image -> Algorithm -> Image
apply img alg
    | alg == OneDimensional = apply1D (grayscale img)
    | alg == FloydSteinberg || alg == JarvisJudiceNinke || alg == Stucki || alg==Atkinson || alg == SierraLite || alg == Burkes = 
            let
                w = width img
                h = height img
                grayImage = grayscale img
                matrix = map (map (\pixel -> fromIntegral (red pixel))) (content grayImage)
                newMatrix = makeMatrix 0 0 alg matrix 255
                newContent = map (map (\pixel -> let newPixel = toEnum pixel in Rgb newPixel newPixel newPixel)) newMatrix
             in 
                Image w h newContent
    | otherwise = error "Sorry, this algorithm isn't supported yet :/"