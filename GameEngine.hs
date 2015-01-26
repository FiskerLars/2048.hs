
module GameEngine where

import System.Random

import Field
import Direction

{-- Execute a movement on a game state --}

moveTransition:: Field -> Direction -> Field
moveTransition f d = case d of
  EAST -> pushEast f
  WEST -> pushWest f
  NORTH -> pushNorth f
  SOUTH -> pushSouth f
  
pushWest = map (\l -> leftShove 0 l)
pushEast = map (\l -> reverse (leftShove 0 (reverse l)))
pushNorth = rotateRight.pushWest.rotateLeft 
pushSouth = rotateLeft.pushWest.rotateRight

leftShove:: Int -> [CellVal] -> [CellVal]
leftShove o [] | o > 0     = (-1):leftShove (o-1) []
               | otherwise = []
leftShove o (x:xs) | x < 0 = leftShove (o+1) $ reduceLeft 0 xs
                   | x > 0 = case null xs of
                     False -> mergeCells o x $ reduceLeft 0 xs
                     True  -> x:leftShove o []


reduceLeft o [] = replicate o (-1)
reduceLeft o (x:xs) | x < 0     = reduceLeft (o+1) xs
                    | otherwise = (x:xs) ++ (replicate o (-1))


mergeCells o x (y:ys) = case x == y of
      True  -> (x+y):(leftShove (o+1) ys)
      False -> x:(leftShove (o) (y:ys))
      


{-- generate a random number (2 or 4) within an empty field (chosen uniformly at random) --}
generateTransition:: RandomGen g => g -> Field ->  Field
generateTransition g f = let (val, posx, posy) = (\(a:x:y:_) -> (a, x-1, y-1))
                                                 $(\(a,g) -> a:randomRs (1,length f) g)
                                                 $ (\(a,g) -> (selectVal a, g))
                                                 $ random g  
                         in replace posy (replace posx val (f!!posy) ) f
                           where
                             selectVal:: Float -> Int
                             selectVal p | p < 0.6 = 2 -- probability of generating a two
                                         | otherwise = 4

                             replace pos elem = (\(a,b) -> a ++ elem:tail b)
                                                .(splitAt pos)


