module Field where

import Data.List

{- A 4x4 field essentially is a list of lists (which is a more general approach than a tuple of tuples).

- Negative values encode empty fields
- Values > 0 encode powers of 2
- Values = 0 are not useful (undefined state)
-}

type Pos = (Int,Int)
type CellVal = Int
type Field=[[CellVal]]
testfield = [[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]]::Field
arbitraryInitField = [ replicate 4 (-1)
                     , [-1,-1,2,-1]
                     , replicate 4 (-1)
                     , [-1,2,2,-1]
                     ]::Field


rotateLeft:: Field -> Field
rotateLeft = reverse.transpose
rotateRight:: Field -> Field
rotateRight = transpose.reverse

