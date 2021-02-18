module Convertions where
import P1
import P2
import P3


makeContent12 :: [[Int]] -> Int -> [[Int]]
makeContent12 matrix maxValue = map (\row -> map (\elem -> if elem == 0 then maxValue else if elem == 1 then 0 else undefined) row) matrix 

convertImage12 :: P1.Image -> P2.Image 
convertImage12 img = 
    let 
        width = P1.width img
        height = P1.height img
        maxValue = 15
        content = makeContent12 (P1.content img) maxValue
    in
        P2.Image width height maxValue content

makeContent13 :: [[Int]] -> [[Rgb]]
makeContent13 = map (\row -> map (\pixel -> if pixel == 0 then Rgb 255 255 255 else if pixel == 1 then Rgb 0 0 0 else undefined) row)

convertImage13 :: P1.Image -> P3.Image 
convertImage13 img = 
    let 
        width = P1.width img
        height = P1.height img
        maxValue = 15
        content = makeContent13 (P1.content img)
    in
        P3.Image width height content


makeContent21 :: [[Int]] -> Int -> [[Int]]
makeContent21 matrix maxValue = map (\row -> map (\elem -> if elem == maxValue then 0 else if elem == 0 then 1 else undefined) row) matrix 

convertImage21 :: P2.Image -> P1.Image 
convertImage21 img = 
    let 
        width = P2.width img
        height = P2.height img
        maxValue = P2.maxValue img
        content = makeContent21 (P2.content img) maxValue
    in
        P1.Image width height content

makeContent23 :: [[Int]] -> Int -> [[Rgb]]
makeContent23 matrix maxValue = map (\row -> map (\pixel -> if pixel == maxValue then Rgb 255 255 255 else if pixel == 0 then Rgb 0 0 0 else undefined) row) matrix

convertImage23 :: P2.Image -> P3.Image 
convertImage23 img = 
    let 
        width = P2.width img
        height = P2.height img
        maxValue = P2.maxValue img
        content = makeContent23 (P2.content img) maxValue
    in
        P3.Image width height content


makeContent31 :: [[Rgb]] -> [[Int]]
makeContent31 = map (\row -> map (\pixel -> if pixel == Rgb 255 255 255 then 0 else if pixel == Rgb 0 0 0 then 1 else undefined) row)

convertImage31 :: P3.Image -> P1.Image 
convertImage31 img = 
    let
        width = P3.width img
        height = P3.height img
        content = makeContent31 (P3.content img)
    in 
        P1.Image width height content

makeContent32 :: [[Rgb]] -> Int -> [[Int]]
makeContent32 matrix maxValue = map (\row -> map (\pixel -> if pixel == Rgb 255 255 255 then maxValue else if pixel == Rgb 0 0 0 then 0 else undefined) row) matrix

convertImage32 :: P3.Image -> P2.Image 
convertImage32 img = 
    let
        width = P3.width img
        height = P3.height img
        maxValue = 15
        content = makeContent32 (P3.content img) maxValue
    in 
        P2.Image width height maxValue content