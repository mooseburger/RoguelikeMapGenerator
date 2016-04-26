module Randomizers where
import Types
import Constants
import System.Random

getRandomElem :: StdGen -> [a] -> (a, StdGen)
getRandomElem gen list = (list !! randomIndex, newGen)
    where (randomIndex, newGen) = randomR (0, length list - 1) gen 

getTwoRandElem :: StdGen -> [a] -> ([a], StdGen)
getTwoRandElem gen [] = ([], gen)
getTwoRandElem gen (e:[]) = (e:[], gen) 
getTwoRandElem gen list = (first:second:[], nextGen)
    where (first, nextGen1) = getRandomElem gen list
          (second, nextGen) = getRandomElem nextGen1 list  

getRandNumElem :: StdGen -> Int -> [a] -> [a] -> ([a], StdGen)
getRandNumElem gen num elems list
    | num > 1 = getRandNumElem newGen (num - 1) (newPos:elems) list
    | otherwise = (newPos:elems, newGen)
    where 
        (newPos, newGen) = getRandomElem gen list

--getRandomPos :: StdGen -> (Pos, StdGen)
--getRandomPos gen = (grid !! randomIndex, newGen)
--    where (randomIndex, newGen) = randomR (0, numPos - 1) gen

randLength :: StdGen -> (Int, StdGen)
randLength gen = randomR (minl, maxl) gen

randWidth :: StdGen -> (Int, StdGen)
randWidth gen = randomR (minw, maxw) gen

randDimensions :: StdGen -> (Int, Int, StdGen)
randDimensions gen = (length, width, newGen2) 
    where
        (length, newGen1) = randLength gen 
        (width, newGen2) = randWidth newGen1
        --(x, newGen3) = randomR (0, 440) newGen2 :: (Int, StdGen)
        --(y, newGen4) = randomR (0, 280) newGen3 :: (Int, StdGen)

cointoss :: StdGen -> (Bool, StdGen)
cointoss gen = random gen

odds :: StdGen -> (Bool, StdGen)
odds gen = if toss <= 1
           then (True, newGen)
           else (False, newGen) 
    where (toss, newGen) = randomR (0, chance) gen :: (Int, StdGen) 
           
randDisplace :: StdGen -> (Int, StdGen)
randDisplace gen = ( displacements !! ind, newGen)
    where (ind, newGen) = randomR (0, numDisp - 1) gen :: (Int, StdGen)
