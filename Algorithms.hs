module Algorithms where

data Algorithm = OneDimensional | FloydSteinberg | JarvisJudiceNinke | 
                 Stucki | Atkinson | Burkes | Sierra | TwoRowSierra |
                 SierraLite | Ordered4x4 | Ordered8x8 deriving (Show,Read,Eq)


data ImgType = P1 | P2 | P3 deriving(Show,Read)

widthMatrix :: [[a]] -> Int
widthMatrix matrix = length (head matrix)
 
getFirst :: (a, b, c) -> a
getFirst (x, _, _) = x

getSecond :: (a, b, c) -> b
getSecond (_,x,_) = x

getThird :: (a, b, c) -> c
getThird (_,_,x) = x


roundPixel :: (Ord b, Num b) => b -> b -> (b, b)
roundPixel num max = if x > num then (0, num) else (max, (-x)) 
    where 
        x = max - num 


clamp :: Ord a => a -> a -> a -> a
clamp x a b = max a (min x b)

extendMatrix :: [[Int]] -> [[Int]] -> Int -> Int -> Int -> [[Int]]
extendMatrix [] _ _ _ _= []
extendMatrix subMatrix matrix row col number
    | row < length matrix = 
        let
            cols = clamp (col-number) 0 (widthMatrix matrix)
        in (take cols (matrix!!row) ++ (head subMatrix) ++ drop (col + widthMatrix subMatrix) (matrix!!row)) : extendMatrix (tail subMatrix) matrix (row + 1) col number
    | otherwise = []


applyError :: Algorithm -> [[Int]] -> Int -> Int -> Int -> Int -> [[Int]]
 
applyError FloydSteinberg matrix row col newPixel error = 
    let
        w = length $ head matrix
        h = length matrix
        multiplier = (fromIntegral error) / 16
        submatrix = [[matrix!!(clamp r 0 h)!!(clamp (col + c) 0 w) | c<-[-1..1]] | r <- [row, row+1]]

        submatrix2 = [[(submatrix!!0!!0), newPixel, (submatrix!!0!!2) + round (7 * multiplier)],
        
                        [(submatrix!!1!!0) + round (3 * multiplier), (submatrix!!1!!1) + round (5 * multiplier), (submatrix!!1!!2) +  (round multiplier)]]  
       
        submatrix3 = 
            [[submatrix2!!r!!c | c <- [0..(widthMatrix submatrix2 -1)], c + col -1 < widthMatrix matrix ] | r <- [0..(length submatrix2-1)], r + row < length matrix]

        croppedMatrix = if col < 1 then  map tail submatrix3 else submatrix3         
        extendedMatrix = extendMatrix croppedMatrix matrix row col 1
    in extendedMatrix

applyError JarvisJudiceNinke matrix row col newPixel error = 
    let
        w = length $ head matrix
        h = length matrix
        multiplier = (fromIntegral error) / 48 :: Float
        submatrix = [[matrix!!(clamp r 0 h)!!(clamp (col + c) 0 w) | c<-[-2..2]] | r <- [row..row+2]]

        submatrix2 = [[(submatrix!!0!!0), submatrix!!0!!1, newPixel, (submatrix!!0!!3) + round (7 * multiplier), (submatrix!!0!!4) + round (5 * multiplier)],
        
                        [(submatrix!!1!!0) + round (3 * multiplier), (submatrix!!1!!1) + round (5 * multiplier), (submatrix!!1!!2) + round (7 * multiplier), (submatrix!!1!!3) + round (5 * multiplier), (submatrix!!1!!4) + round (3 * multiplier)],
                        [(submatrix!!2!!0) + round multiplier, (submatrix!!2!!1) + round (3 * multiplier), (submatrix!!2!!2) + round (5 * multiplier), (submatrix!!2!!3) + round (3 * multiplier), (submatrix!!2!!4) + round multiplier]]  
       
        submatrix3 = 
            [[submatrix2!!r!!c | c <- [0..(widthMatrix submatrix2 -1)], c + col -2 < widthMatrix matrix ] | r <- [0..(length submatrix2-1)], r + row < length matrix]

        croppedMatrix = if col == 0 then  map (tail . tail) submatrix3 else if col == 1 then map tail submatrix3 else submatrix3         
        extendedMatrix = extendMatrix croppedMatrix matrix row col 2 
    in extendedMatrix

applyError Stucki matrix row col newPixel error = 
    let
        w = length $ head matrix
        h = length matrix
        multiplier = (fromIntegral error) / 42 :: Float
        submatrix = [[matrix!!(clamp r 0 h)!!(clamp (col + c) 0 w) | c<-[-2..2]] | r <- [row..row+2]]

        submatrix2 = [[(submatrix!!0!!0), submatrix!!0!!1, newPixel, (submatrix!!0!!3) + round (8 * multiplier), (submatrix!!0!!4) + round (4 * multiplier)],
        
                        [(submatrix!!1!!0) + round (2 * multiplier), (submatrix!!1!!1) + round (4 * multiplier), (submatrix!!1!!2) + round (8 * multiplier), (submatrix!!1!!3) + round (4 * multiplier), (submatrix!!1!!4) + round (2 * multiplier)],
                        [(submatrix!!2!!0) + round multiplier, (submatrix!!2!!1) + round (2 * multiplier), (submatrix!!2!!2) + round (4 * multiplier), (submatrix!!2!!3) + round (2 * multiplier), (submatrix!!2!!4) + round multiplier]]  
       
        submatrix3 = 
            [[submatrix2!!r!!c | c <- [0..(widthMatrix submatrix2 -1)], c + col -2 < widthMatrix matrix ] | r <- [0..(length submatrix2-1)], r + row < length matrix]

        croppedMatrix = if col == 0 then  map (tail . tail) submatrix3 else if col == 1 then map tail submatrix3 else submatrix3         

        extendedMatrix = extendMatrix croppedMatrix matrix row col 2   
    in extendedMatrix

applyError Atkinson matrix row col newPixel error = 
    let
        w = length $ head matrix
        h = length matrix
        multiplier = (fromIntegral error) / 8 :: Float
        submatrix = [[matrix!!(clamp r 0 h)!!(clamp (col + c) 0 w) | c<-[-1..2]] | r <- [row..row+2]]

        submatrix2 = [[(submatrix!!0!!0), newPixel, (submatrix!!0!!2) + round multiplier, (submatrix!!0!!3) + round multiplier],
        
                        [(submatrix!!1!!0) + round multiplier, (submatrix!!1!!1) + round multiplier, (submatrix!!1!!2) + round multiplier, (submatrix!!1!!3)],
                        [(submatrix!!2!!0), (submatrix!!2!!1) + round multiplier, (submatrix!!2!!2), (submatrix!!2!!3)]]  
       
        submatrix3 = 
            [[submatrix2!!r!!c | c <- [0..(widthMatrix submatrix2 -1)], c + col -1 < widthMatrix matrix ] | r <- [0..(length submatrix2-1)], r + row < length matrix]

        croppedMatrix = if col < 1 then  map tail submatrix3 else submatrix3         
        extendedMatrix = extendMatrix croppedMatrix matrix row col 1        
    in extendedMatrix

applyError Burkes matrix row col newPixel error = 
    let
        w = length $ head matrix
        h = length matrix
        multiplier = (fromIntegral error) / 32 :: Float
        submatrix = [[matrix!!(clamp r 0 h)!!(clamp (col + c) 0 w) | c<-[-2..2]] | r <- [row..row+1]]

        submatrix2 = [[(submatrix!!0!!0), (submatrix!!0!!1), newPixel, (submatrix!!0!!3) + round (8 * multiplier), (submatrix!!0!!4) + round (4 * multiplier)],
        
                        [(submatrix!!1!!0) + round (2 * multiplier), (submatrix!!1!!1) + round (4 * multiplier), (submatrix!!1!!2) + round (8 * multiplier), (submatrix!!1!!3) + round (4 * multiplier), (submatrix!!1!!4) + round (2 * multiplier)]]
       
        submatrix3 = 
            [[submatrix2!!r!!c | c <- [0..(widthMatrix submatrix2 -1)], c + col -2 < widthMatrix matrix ] | r <- [0..(length submatrix2-1)], r + row < length matrix]

        croppedMatrix = if col == 0 then  map (tail.tail) submatrix3 else if col == 1 then map tail submatrix3 else submatrix3         
     
        extendedMatrix = extendMatrix croppedMatrix matrix row col 2        
    in extendedMatrix

applyError SierraLite  matrix row col newPixel error = 
    let
        w = length $ head matrix
        h = length matrix
        multiplier = (fromIntegral error) / 4 :: Float
        submatrix = [[matrix!!(clamp r 0 h)!!(clamp (col + c) 0 w) | c<-[-1..1]] | r <- [row..row+1]]

        submatrix2 = [[(submatrix!!0!!0), newPixel, (submatrix!!0!!2) + round (2 * multiplier)],
        
                        [(submatrix!!1!!0) + round multiplier, (submatrix!!1!!1) + round multiplier, (submatrix!!1!!2)]]  
       
        submatrix3 = 
            [[submatrix2!!r!!c | c <- [0..(widthMatrix submatrix2 -1)], c + col -1 < widthMatrix matrix ] | r <- [0..(length submatrix2-1)], r + row < length matrix]

        croppedMatrix = if col < 1 then  map tail submatrix3 else submatrix3         
        extendedMatrix = extendMatrix croppedMatrix matrix row col 1        
    in extendedMatrix


makeMatrix :: Int -> Int -> Algorithm -> [[Int]] -> Int -> [[Int]]
makeMatrix row col alg matrix max 
    | row < length matrix && col < widthMatrix matrix =          
        let 
            currentPixel = matrix!!row!!col
            pair = roundPixel currentPixel max 
            error =  snd pair
            newPixel = fst pair
            subMatrix = applyError alg matrix row col newPixel error
            combineMatrices = take row matrix ++ subMatrix ++ drop (row + length subMatrix) matrix
        in
            makeMatrix row (col + 1) alg combineMatrices max
    | row <  length matrix = makeMatrix (row + 1) 0 alg matrix max 
    | otherwise = matrix

