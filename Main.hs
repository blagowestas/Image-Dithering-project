module Main where
import Data.Word ( Word8 )
import Convertions
import P1
import P2
import P3

import Algorithms 

findType :: String -> ImgType 
findType str = 
    let
        fileExtension = reverse (takeWhile (/='.') (reverse str))
    in  
        if fileExtension == "pbm" then P1 
        else if fileExtension == "pgm" then P2 
        else if fileExtension == "ppm" then P3 
        else error "Unknown file format" 



dither :: ImgType -> ImgType -> String -> String -> Algorithm -> IO ()
--file inputType outputType path newPath 
dither P1 P2 path newPath alg = do
                                img <- P1.loadImage path
                                let newImg = P1.apply img alg 
                                let convertedImg = convertImage12 newImg
                                P2.saveImage newPath convertedImg

dither P1 P3 path newPath alg = do
                                img <- P1.loadImage path
                                let newImg = P1.apply img alg
                                let convertedImg = convertImage13 newImg
                                P3.saveImage newPath convertedImg

dither P2 P1 path newPath alg = do
                                img <- P2.loadImage path
                                let newImg = P2.apply img alg 
                                let convertedImg = convertImage21 newImg
                                P1.saveImage newPath convertedImg

dither P2 P3 path newPath alg = do
                                img <- P2.loadImage path
                                let newImg = P2.apply img alg
                                let convertedImg = convertImage23 newImg
                                P3.saveImage newPath convertedImg

dither P3 P1 path newPath alg = do
                                img <- P3.loadImage path
                                let newImg = P3.apply img alg
                                let convertedImg = convertImage31 newImg
                                P1.saveImage newPath convertedImg

dither P3 P2 path newPath alg = do
                                img <- P3.loadImage path
                                let newImg = P3.apply img alg 
                                let convertedImg = convertImage32 newImg
                                P2.saveImage newPath convertedImg

dither P1 P1 path newPath alg = do 
                                img <- P1.loadImage path
                                let newImg = P1.apply img alg
                                P1.saveImage newPath newImg

dither P2 P2 path newPath alg = do 
                                img <- P2.loadImage path
                                let newImg = P2.apply img alg
                                P2.saveImage newPath newImg

dither P3 P3 path newPath alg = do 
                                img <- P3.loadImage path
                                let newImg = P3.apply img alg
                                P3.saveImage newPath newImg


getAlg :: String -> Algorithm 
getAlg "OneDimensional"    = OneDimensional 
getAlg "FloydSteinberg"    = FloydSteinberg 
getAlg "JarvisJudiceNinke" = JarvisJudiceNinke
getAlg "Stucki"            = Stucki
getAlg "Atkinson"          = Atkinson
getAlg "Burkes"            = Burkes
getAlg "Sierra"            = Sierra
getAlg "TwoRowSierra"      = TwoRowSierra
getAlg "SierraLite"        = SierraLite 
getAlg "Ordered4x4"        = Ordered4x4
getAlg "Ordered8x8"        = Ordered8x8
getAlg _                   = error "Wrong algorithm"


main :: IO ()
main = do 
    putStrLn "Insert input path: "
    path <- getLine
    putStrLn "Insert output path: "
    newPath <- getLine
    putStrLn "Insert algorithm: "
    algStr <- getLine 
    let 
        imgType = findType path
        newType = findType newPath
        alg = getAlg algStr
    dither imgType newType path newPath alg